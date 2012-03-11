package pl.luckboy.liftchess.engine

/** Klasa planszy. 
 * 
 * @author Łukasz Szpakowski
 */
class Board private(
    pieces: Seq[SidePieceOption],
    private var mSide: Side,
    castlingPair: (Castling, Castling),
    private var mEnPassant: SquareOption,
    private var mHalfmoveClock: Int,
    private var mFullmoveNumber: Int
    )
{
  import Board._
  
  require(pieces.size == 64)
  require(pieces.count { _ == SidePieceOption.WhiteKing } == 1 && pieces.count { _ == SidePieceOption.BlackKing } == 1)
  
  // Sprawdzenie czy en passant jest dobrze ustawiony.
  require(mEnPassant.foldLeft(true) {  
    (_, enpSq) =>
      val (enpRow, prevDstRow) = side match {
        case Side.White => (2, 3)
        case Side.Black => (5, 4)
      }
      val pawn = SidePieceOption.fromSideAndPiece(side, Piece.Pawn)
      val prevDst = Square(prevDstRow, Square.toColumn(enpSq))
      Square.toRow(enpSq) == enpRow &&
      pieces(prevDst) == SidePieceOption.fromSideAndPiece(side.opposite, Piece.Pawn) &&
      Square.foldPawnCaptureSquares(enpSq, side.opposite)(false) { (b, _) => !b } { (_, sq) => pieces(sq) == pawn }
  })
  
  // Sprawdzenie czy dobrze ustawiono roszady.
  Seq((Side.White, castlingPair._1, 7), (Side.Black, castlingPair._2, 0)).foreach {
    case (side, castling, row) =>
      if(castling != Castling.NoneCastling) 
        require(pieces(Square(row, 4)) == SidePieceOption.fromSideAndPiece(side, Piece.King))
      if((castling & Castling.KingsideCastling) != Castling.NoneCastling)
        require(pieces(Square(row, 7)) == SidePieceOption.fromSideAndPiece(side, Piece.Rook))
      if((castling & Castling.QueensideCastling) != Castling.NoneCastling)
        require(pieces(Square(row, 0)) == SidePieceOption.fromSideAndPiece(side, Piece.Rook))
  }

  require(mHalfmoveClock >= 0)
  require(mFullmoveNumber >= 1)
  
  /** Tablica bierek. */
  private val mPieces = pieces.toArray
  
  /** Tablica dla roszad. Zawiera znaczniki roszad w odpowiednich pozycjach. */
  private val mCastlingArray = (
      Array(castlingPair._2 & Castling.QueensideCastling) ++
      Array.fill(6)(Castling.NoneCastling) ++
      Array(castlingPair._2 & Castling.KingsideCastling) ++ 
      Array.fill(6 * 8)(Castling.NoneCastling) ++
      Array(castlingPair._1 & Castling.QueensideCastling) ++
      Array.fill(6)(Castling.NoneCastling) ++
      Array(castlingPair._1 & Castling.KingsideCastling)
      )
  
  /** Lista statyczna zawierająca pola poszczególnych bierek. */
  private val mSList = {
    val slist = Array.fill(40)(-1)
    val lastSListIndexes = Side.makeArray(
        StartSListIndexes(Side.White.id).clone(),
        StartSListIndexes(Side.Black.id).clone()
        )
    
    (0 to 63).foreach {
      sq => mPieces(sq).foldLeft(()) {
        (_, sp) =>
          val side = sp.side
          val piece = sp.piece
          val pieceId = if(lastSListIndexes(side.id)(piece.id) < EndSListIndexes(side.id)(piece.id)) piece.id else Piece.Pawn.id
          require(lastSListIndexes(side.id)(pieceId) < EndSListIndexes(side.id)(pieceId))
          slist(lastSListIndexes(side.id)(pieceId)) = sq
          lastSListIndexes(side.id)(pieceId) += 1
        }
    }
    slist
  }

  /** Indeksy do listy statycznej w postaci mapy (pole -> indeks do listy). */
  private val mSListIndexes = {
    val slistIndexes = Array.fill(64)(-1)

    (0 to 39).foreach { i => if(mSList(i) != -1) { slistIndexes(mSList(i)) = i } }
    slistIndexes
  }  
  
  /** Hash key. */
  private var mHashKey = 0L
  
  /** Podaje liczbę wystąpień danej bierki danej strony.
   * @param side		strona.
   * @param piece		bierka.
   * @return			liczba bierek.
   */
  def countSidePieces(side: Side, piece: Piece): Int = {
    if(piece == Piece.King) {
      1
    } else {
      var sum = 0
      if(piece != Piece.Pawn) {
        val j = StartSListIndexes(side.id)(piece.id)
        if(mSList(j) != -1) sum += 1
        if(mSList(j + 1) != -1) sum += 1
      }
      var i = StartSListIndexes(side.id)(Piece.Pawn.id)
      val n = EndSListIndexes(side.id)(Piece.Pawn.id)
      while(i < n) {
        if(mSList(i) != -1 && mPieces(mSList(i)).isPiece(piece)) sum += 1
        i += 1
      }
      sum
    }
  }
  
  /** Podaje liczbę wystąpień bierki.
   * @param piece		bierka.
   * @return			liczba bierek.
   */
  def countPieces(piece: Piece): Int = {
    var sum = 0
    var i = 0
    while(i < 2) {
      sum += countSidePieces(Sides(i), piece)
      i += 1
    }
    sum
  }
  
  /** Podaje liczbę wszystkich bierek danej strony.
   * @param side		strona.
   * @return			liczba bierek.
   */
  def countAllSidePieces(side: Side): Int = {
    var sum = 0
    var i = MinStartSListIndexes(side.id)
    val n = MaxEndSListIndexes(side.id)
    while(i < n) {
      if(mSList(i) != -1) sum += 1
      i += 1
    }
    sum
  }
  
  /** Podaje liczbę wszystkich bierek.
   * @return			liczba bierek.
   */
  def countAllPieces: Int = {
    var sum = 0
    var i = 0
    while(i < 2) {
      sum += countAllSidePieces(Sides(i))
      i += 1
    }
    sum
  }
  
  /** Składa określone bierki danej strony.
   * @param side		strona.
   * @param piece		bierka.
   * @param z			wartość początkowa.
   * @param p			funkcja przerwania (gdy false przerywa).
   * @param f			funkcja składania.
   * @return			wynik składania.
   */
  def foldSidePieces[T](side: Side, piece: Piece)(z: T)(p: (T, Int) => Boolean)(f: (T, Int) => T): T = {
    if(piece == Piece.King) {
      val sq = mSList(StartSListIndexes(side.id)(Piece.King.id))
      if(p(z, sq)) f(z, sq)  else z
    } else {
      var y = z
      if(piece != Piece.Pawn) {
        val j = StartSListIndexes(side.id)(piece.id)
        if(mSList(j) != -1) {
          val sq = mSList(j)
          if(!p(y, sq)) return y
          y = f(y, sq)
        }
        if(mSList(j + 1) != -1) {
          val sq = mSList(j + 1)
          if(!p(y, sq)) return y
          y = f(y, sq)
        }
      }
      var i = StartSListIndexes(side.id)(Piece.Pawn.id)
      val n = EndSListIndexes(side.id)(Piece.Pawn.id)
      while(i < n) {
        if(mSList(i) != -1 && mPieces(mSList(i)).isPiece(piece)) {
          val sq = mSList(i) 
          if(!p(y, sq)) return y
          y = f(y, sq)
        }
        i += 1
      }
      y
    }
  }
  
  /** Składa określone bierki.
   * @param piece		bierka.
   * @param z			wartość początkowa.
   * @param p			funkcja przerwania (gdy false przerywa).
   * @param f			funkcja składania.
   * @return			wynik składania.
   */
  def foldPieces[T](piece: Piece)(z: T)(p: (T, Int) => Boolean)(f: (T, Int) => T): T = {
    if(piece == Piece.King) {
      val sq = mSList(StartSListIndexes(Side.White.id)(Piece.King.id))
      if(p(z, sq)) {
        val y = f(z, sq)
        val sq2 = mSList(StartSListIndexes(Side.Black.id)(Piece.King.id))
        if(p(y, sq2)) f(y, sq2) else y
      } else {
        z
      }
    } else {
      var y = z
      var k = 0
      while(k < 2) {
        val side = Sides(k)
        if(piece != Piece.Pawn) {
          var j = StartSListIndexes(side.id)(piece.id)
          if(mSList(j) != -1) {
            val sq = mSList(j)
            if(!p(y, sq)) return y
            y = f(y, sq)
          }
          if(mSList(j + 1) != -1) {
            val sq = mSList(j + 1)
            if(!p(y, sq)) return y
            y = f(y, sq)
          }
        }
        var i = StartSListIndexes(side.id)(Piece.Pawn.id)
        val n = EndSListIndexes(side.id)(Piece.Pawn.id)
        while(i < n) {
          if(mSList(i) != -1 && mPieces(mSList(i)).isPiece(piece)) {
            val sq = mSList(i)
            if(!p(y, sq)) return y
            y = f(y, sq)
          }
          i += 1
        }
        k += 1
      }
      y
    }
  }
  
  /** Składa wszystkie bierki danej strony.
   * @param side		strona.
   * @param z			wartość początkowa.
   * @param p			funkcja przerwania (gdy false przerywa).
   * @param f			funkcja składania.
   * @return			wynik składania.
   */
  def foldAllSidePieces[T](side: Side)(z: T)(p: (T, Int) => Boolean)(f: (T, Int) => T): T = {
    var y = z
    var i = MinStartSListIndexes(side.id)
    val n = MaxEndSListIndexes(side.id)
    while(i < n) {
      if(mSList(i) != -1) {
        val sq = mSList(i)
        if(!p(y, sq)) return y
        y = f(y, sq)
      }
      i += 1
    }
    y
  }

  /** Składa wszystkie bierki.
   * @param z			wartość początkowa.
   * @param p			funkcja przerwania (gdy false przerywa).
   * @param f			funkcja składania.
   * @return			wynik składania.
   */
  def foldAllPieces[T](z: T)(p: (T, Int) => Boolean)(f: (T, Int) => T): T = {
    var y = z
    var k = 0
    while(k < 2) {
      val side = Sides(k)
      var i = MinStartSListIndexes(side.id)
      val n = MaxEndSListIndexes(side.id)
      while(i < n) {
        if(mSList(i) != -1) {
          val sq = mSList(i)
          if(!p(y, sq)) return y
          y = f(y, sq)
        }
        i += 1
      }
      k += 1
    }
    y
  }

  /** Strona. */
  def side: Side =
    mSide

  /** Dostępne roszady dla danej strony.
   * @param side		strona.
   * @return			dostępne roszady.
   */
  def castling(side: Side): Castling = {
    val row = if(side == Side.White) 7 else 0
    mCastlingArray(Square(row, 0)) | mCastlingArray(Square(row, 7))
  }
  
  /** Pole bicia w przelocie. */
  def enPassant: SquareOption =
    mEnPassant

  /** Liczba półruchów. */
  def halfmoveClock: Int =
    mHalfmoveClock

  /** Liczna ruchów. */
  def fullmoveNumber: Int =
    mFullmoveNumber
  
  /** Hashowy klucz. */
  def hashKey: Long = throw new Exception
  
  /** Podaje bierkę na określonym polu. */
  def apply(sq: Int): SidePieceOption =
    mPieces(sq)

  /** Podobnie jak unsafeUndoNormalMoveOrCaptureWithoutHashKey tylko z obliczaniem hashkeya.
   * @param move		ruch.
   * @return 			dane używane do cofania ruchu lub -1 jeśli nie wykonano ruchu.
   */
  private def unsafeMakeNormalMoveOrCapture(move: Move) =
    unsafeMakeNormalMoveOrCaptureWithoutHashKey(move)
     
  /** Podobnie jak unsafeMakeEnPassantWithoutHashKey tylko z obliczaniem hashkeya.
   * @param move		ruch.
   * @return 			dane używane do cofania ruchu lub -1 jeśli nie wykonano ruchu.
   */
  private def unsafeMakeEnPassant(move: Move) =
    unsafeMakeEnPassantWithoutHashKey(move)

  /** Podobnie jak unsafeMakeCastlingWithoutHashKey tylko z obliczeniem hashkey.
   * @param move		ruch.
   * @return 			dane używane do cofania ruchu lub -1 jeśli nie wykonano ruchu.
   */
  private def unsafeMakeCastling(move: Move) =
    unsafeMakeCastlingWithoutHashKey(move)

  /** Słada następnik planszy. W rzeczywistości wykonuje ruch jeśli jest legalny i wykonuje daną funkcje. Po 
   * wykonaniu funkcji cofa ruch.
   * @param	move		ruch.
   * @param z			wartość początkowa.
   * @param f			funkcja składania.
   * @return			wynik funkcji lub wartość początkowa.
   */
  def unsafeFoldSuccessor[T](move: Move)(z: T)(f: (T, Board) => T): T =
    unsafeFoldSuccessorWithoutHashKey(move)(z)(f)

  /** Słada następnik planszy dla ruchu pustego. W rzeczywistości  wykonuje pusty ruch jeśli jest legalny i 
   * wykonuje daną funkcje. Po wykonaniu funkcji cofa ruch pusty.
   * @param z			wartość początkowa.
   * @param f			funkcja.
   * @return			wynik funkcji lub wartość początkowa.
   */  
  def unsafeFoldNullSuccessor[T](z: T)(f: (T, Board) => T): T =
    unsafeFoldNullSuccessorWithoutHashKey(z)(f)
  
  /** Wykonuje normalny ruch lub bicie (nie jest to bicie w przelocie lub roszada) bez obliczania hash keya.
   * @param move		ruch.
   * @return 			dane używane do cofania ruchu lub -1 jeśli nie wykonano ruchu.
   */
  private def unsafeMakeNormalMoveOrCaptureWithoutHashKey(move: Move) = {
    val piece = move.piece
    val src = move.source
    val dst = move.destination
    val savedDstPiece = mPieces(dst)
    
    // Bierki.
    mPieces(dst) = SidePieceOption.fromSideAndPiece(side, move.promotionPiece.foldLeft(piece) { (_, promPiece) => promPiece })
    mPieces(src) = SidePieceOption.None
    if(!inCheckNoCache) {
      val savedWhiteCastling = mCastlingArray(Square(7, 0)) | mCastlingArray(Square(7, 7))
      val savedBlackCastling = mCastlingArray(Square(0, 0)) | mCastlingArray(Square(0, 7))
      val savedDstSListIndex = mSListIndexes(dst)
      
      // Lista.
      if(mSListIndexes(dst) != -1) mSList(mSListIndexes(dst)) = -1
      mSList(mSListIndexes(src)) = dst
      mSListIndexes(dst) = mSListIndexes(src)
      mSListIndexes(src) = -1
      // Roszady.
      mCastlingArray(dst) = Castling.NoneCastling
      mCastlingArray(src) = Castling.NoneCastling
      piece match {
        case Piece.Pawn =>
          // Ustawia dla pola en passant.
          val dstRow = if(side == Side.White) 4 else 3
          val col = Square.toColumn(dst)
          val enpSq = Square(if(side == Side.White) 5 else 2, col)
          val pawn = SidePieceOption.fromSideAndPiece(side.opposite, Piece.Pawn) 
          
          if(Square.toRow(dst) == dstRow && Square.foldPawnCaptureSquares(enpSq, side)(false) { (b, _) => !b } { (_, sq) => this(sq) == pawn } ) {
        	mEnPassant = SquareOption(enpSq)
          } else {
            mEnPassant = SquareOption.None
          }
          // Wzerowanie liczby półruchów.
          mHalfmoveClock = 0
        case Piece.King =>
          val row = if(side == Side.White) 7 else 0
          // Ustawia roszady (usuwa je).
          mCastlingArray(Square(row, 0)) = Castling.NoneCastling
          mCastlingArray(Square(row, 7)) = Castling.NoneCastling
          // Ustawia dla pola en passant.
          mEnPassant = SquareOption.None
          // Zwiękrzenie liczby półruchów.
          mHalfmoveClock = if(savedDstPiece == SidePieceOption.None) mHalfmoveClock + 1 else 0
        case _          =>
          // Ustawia dla pola en passant.
          mEnPassant = SquareOption.None
          // Zwiękrzenie liczby półruchów.
          mHalfmoveClock = if(savedDstPiece == SidePieceOption.None) mHalfmoveClock + 1 else 0
      }
      // Liczba ruchów.
      if(side == Side.Black) mFullmoveNumber += 1
      // Strona.
      mSide = mSide.opposite
      savedWhiteCastling.id | (savedBlackCastling.id << 4) | ((savedDstSListIndex & 255) << 8) | (savedDstPiece.id << 16)
    } else {
      mPieces(src) = SidePieceOption.fromSideAndPiece(side, piece)
      mPieces(dst) = savedDstPiece
      -1
    }
  }
  
  /** Cofa normalny ruch lub bicie (nie jest to bicie w przelocie lub roszada) bez obliczania hash keya.
   * @param move				ruch.
   * @param undo				dane używane do cofania ruchu.
   * @param savedEnPassant		zachowane pole en passant.
   * @param savedHalfmoveClock	zachowena liczba półruchów od bicia lub ruchu pionkiem.
   */
  private def unsafeUndoNormalMoveOrCaptureWithoutHashKey(move: Move, undo: Int, savedEnPassant: SquareOption, savedHalfmoveClock: Int) = {
    val piece = move.piece
    val src = move.source
    val dst = move.destination
    val savedWhiteCastling = Castling(undo & 15)
    val savedBlackCastling = Castling((undo >> 4) & 15)
    val savedDstSListIndex = (undo << 16) >> 24
    val savedDstPiece = SidePieceOption(undo >> 16)

    // Strona.
    mSide = mSide.opposite
    // Liczba ruchów i półruchów.
    if(side == Side.Black) mFullmoveNumber -= 1
    mHalfmoveClock = savedHalfmoveClock
    // Pole en passant.
    mEnPassant = savedEnPassant
    // Roszady.
    mCastlingArray(Square(7, 7)) = savedWhiteCastling & Castling.KingsideCastling
    mCastlingArray(Square(7, 0)) = savedWhiteCastling & Castling.QueensideCastling
    mCastlingArray(Square(0, 7)) = savedBlackCastling & Castling.KingsideCastling
    mCastlingArray(Square(0, 0)) = savedBlackCastling & Castling.QueensideCastling
    // Lista.
    mSListIndexes(src) = mSListIndexes(dst)
    mSListIndexes(dst) = savedDstSListIndex
    mSList(mSListIndexes(src)) = src
    if(mSListIndexes(dst) != -1) mSList(mSListIndexes(dst)) = dst
    // Bierki.
    mPieces(src) = SidePieceOption.fromSideAndPiece(side, piece)
    mPieces(dst) = savedDstPiece
  }

  /** Wykonuje bicie w przelocie bez obliczania hash keya.
   * @param move		ruch.
   * @return			dane uzywane do cofania ruchu lub -1 jeśli nie wykonano ruchu.
   */
  private def unsafeMakeEnPassantWithoutHashKey(move: Move) = {
    val src = move.source
    val dst = move.destination
    val capSq = Square(Square.toRow(src), Square.toColumn(dst))
    
    // Bierki.
    mPieces(dst) = SidePieceOption.fromSideAndPiece(side, Piece.Pawn)
    mPieces(src) = SidePieceOption.None
    mPieces(capSq) = SidePieceOption.None
    if(!inCheckNoCache) {
      val savedCapSListIndex = mSListIndexes(capSq)
      
      // Lista.
      mSList(mSListIndexes(src)) = dst
      mSListIndexes(dst) = mSListIndexes(src)
      mSListIndexes(src) = -1
      mSList(mSListIndexes(capSq)) = -1
      mSListIndexes(capSq) = -1
      // Pole en passant.
      mEnPassant = SquareOption.None
      // Liczba ruchów i półruchów.
      mHalfmoveClock = 0
      if(side == Side.Black) mFullmoveNumber += 1
      // Strona
      mSide = mSide.opposite
      savedCapSListIndex & 255
    } else {
      mPieces(capSq) = SidePieceOption.fromSideAndPiece(side.opposite, Piece.Pawn)
      mPieces(dst) = SidePieceOption.None
      mPieces(src) = SidePieceOption.fromSideAndPiece(side, Piece.Pawn)
      -1
    }
  }
  
  /** Cofa bicie w przelocie bez obliczania hash keya.
   * @param move				ruch.
   * @param undo				dane używane do cofania ruchu.
   * @param savedEnPassant		zachowane pole en passant.
   * @param savedHalfmoveClock	zachowena liczba półruchów od bicia lub ruchu pionkiem.
   */
  private def unsafeUndoEnPassantWithoutHashKey(move: Move, undo: Int, savedEnPassant: SquareOption, savedHalfmoveClock: Int) = {
    val src = move.source
    val dst = move.destination
    val capSq = Square(Square.toRow(src), Square.toColumn(dst))
    val savedCapSListIndex = (undo << 24) >> 24

    // Strona.
    mSide = mSide.opposite
    // Liczba ruchów i półruchów.
    if(side == Side.Black) mFullmoveNumber -= 1
    mHalfmoveClock = savedHalfmoveClock
    // Pole en passant.
    mEnPassant = savedEnPassant
    // Lista.
    mSListIndexes(src) = mSListIndexes(dst)
    mSListIndexes(dst) = -1
    mSList(mSListIndexes(src)) = src
    mSListIndexes(capSq) = savedCapSListIndex
    mSList(mSListIndexes(capSq)) = capSq
    // Bierki.
    mPieces(capSq) = SidePieceOption.fromSideAndPiece(side.opposite, Piece.Pawn)
    mPieces(dst) = SidePieceOption.None
    mPieces(src) = SidePieceOption.fromSideAndPiece(side, Piece.Pawn)
  }

  /** Wykokuje roszadę bez obliczania hash keya.
   * @param move		ruch.
   * @return 			dane uzywane do cofania ruchu lub -1 jeśli nie wykonano ruchu.
   */
  private def unsafeMakeCastlingWithoutHashKey(move: Move) = {
    val row = if(side == Side.White) 7 else 0
    val kingSrc = Square(row, move.source)
    val kingDst = Square(row, move.destination)
    val rookSrc = Square(row, if(move.moveType == MoveType.KingsideCastling) 7 else 0)
    val rookDst = Square(row, if(move.moveType == MoveType.KingsideCastling) 5 else 3)
    
    if(!inCheck && !attack(rookDst, side.opposite) && !attack(kingDst, side.opposite)) {
      val savedCastling = mCastlingArray(Square(row, 0)) | mCastlingArray(Square(row, 7))
      
      // Bierki.
      mPieces(kingDst) = SidePieceOption.fromSideAndPiece(side, Piece.King)
      mPieces(kingSrc) = SidePieceOption.None
      mPieces(rookDst) = SidePieceOption.fromSideAndPiece(side, Piece.Rook)
      mPieces(rookSrc) = SidePieceOption.None
      // Lista.
      mSList(mSListIndexes(kingSrc)) = kingDst
      mSListIndexes(kingDst) = mSListIndexes(kingSrc)
      mSListIndexes(kingSrc) = -1
      mSList(mSListIndexes(rookSrc)) = rookDst
      mSListIndexes(rookDst) = mSListIndexes(rookSrc)
      mSListIndexes(rookSrc) = -1
      // Roszada.
      mCastlingArray(Square(row, 7)) = Castling.NoneCastling
      mCastlingArray(Square(row, 0)) = Castling.NoneCastling
      // Pole en passant.
      mEnPassant = SquareOption.None
      // Liczba ruchów oraz półruchów.
      mHalfmoveClock += 1
      if(side == Side.Black) mFullmoveNumber += 1
      // Strona.
      mSide = mSide.opposite
      savedCastling.id
    } else {
      -1
    }
  }
  
  /** Cofa roszadę bez obliczania hash keya.
   * @param move				ruch.
   * @param undo				dane używane do cofania ruchu.
   * @param savedEnPassant		zachowane pole en passant.
   * @param savedHalfmoveClock  zachowana liczna półruchów od ruchu pionka lub bicia.
   */
  private def unsafeUndoCastlingWithoutHashKey(move: Move, undo: Int, savedEnPassant: SquareOption, savedHalfmoveClock: Int) = {
    val row = if(side == Side.Black) 7 else 0
    val kingSrc = Square(row, move.source)
    val kingDst = Square(row, move.destination)
    val rookSrc = Square(row, if(move.moveType == MoveType.KingsideCastling) 7 else 0)
    val rookDst = Square(row, if(move.moveType == MoveType.KingsideCastling) 5 else 3)
    val savedCastling = Castling(undo)

    // Strona.
    mSide = mSide.opposite
    // Liczba ruchów oraz półruchów.
    if(side == Side.Black) mFullmoveNumber -= 1
    mHalfmoveClock = savedHalfmoveClock
    // Pole en passant.
    mEnPassant = savedEnPassant
    // Roszady.
    mCastlingArray(Square(row, 7)) = savedCastling & Castling.KingsideCastling
    mCastlingArray(Square(row, 0)) = savedCastling & Castling.QueensideCastling
    // Lista.
    mSListIndexes(kingSrc) = mSListIndexes(kingDst)
    mSListIndexes(kingDst) = -1
    mSList(mSListIndexes(kingSrc)) = kingSrc
    mSListIndexes(rookSrc) = mSListIndexes(rookDst)
    mSListIndexes(rookDst) = -1
    mSList(mSListIndexes(rookSrc)) = rookSrc
    // Bierki.
    mPieces(kingSrc) = SidePieceOption.fromSideAndPiece(side, Piece.King)
    mPieces(kingDst) = SidePieceOption.None
    mPieces(rookSrc) = SidePieceOption.fromSideAndPiece(side, Piece.Rook)
    mPieces(rookDst) = SidePieceOption.None
  }
  
  /** Słada następnik planszy bez obliczania hash klucza. W rzeczywistości wykonuje ruch jeśli jest legalny i 
   * wykonuje daną funkcje. Po wykonaniu funkcji cofa ruch.
   * @param	move		ruch.
   * @param z			wartość początkowa.
   * @param f			funkcja składania.
   * @return			wynik funkcji lub wartość początkowa.
   */
  def unsafeFoldSuccessorWithoutHashKey[T](move: Move)(z: T)(f: (T, Board) => T): T = {
    val savedEnPassant = mEnPassant
    val savedHalfmoveClock = mHalfmoveClock
    move.moveType match {
      case MoveType.NormalMove | MoveType.Capture =>
        val undo = unsafeMakeNormalMoveOrCaptureWithoutHashKey(move)
        if(undo != -1) {
          val y = f(z, this)
          unsafeUndoNormalMoveOrCaptureWithoutHashKey(move, undo, savedEnPassant, savedHalfmoveClock)
          y
        } else {
          z
        }
      case MoveType.EnPassant                     =>
        val undo = unsafeMakeEnPassantWithoutHashKey(move)
        if(undo != -1) {
          val y = f(z, this)
          unsafeUndoEnPassantWithoutHashKey(move, undo, savedEnPassant, savedHalfmoveClock)
          y
        } else {
          z
        }
      case _                                      =>
        val undo = unsafeMakeCastlingWithoutHashKey(move)
        if(undo != -1) {
          val y = f(z, this)
          unsafeUndoCastlingWithoutHashKey(move, undo, savedEnPassant, savedHalfmoveClock)
          y
        } else {
          z
        }
    }
  }

  /** Słada następnik planszy dla ruchu pustego bez obliczania hash klucza. W rzeczywistości wykonuje pusty ruch 
   * jeśli jest legalnyi wykonuje daną funkcje. Po wykonaniu funkcji cofa ruch pusty.
   * @param z			wartość początkowa.
   * @param f			funkcja składania.
   * @return			wynik funkcji lub wartość początkowa.
   */  
  def unsafeFoldNullSuccessorWithoutHashKey[T](z: T)(f: (T, Board) => T): T = {
    mSide = mSide.opposite
    val y = f(z, this)
    mSide = mSide.opposite
    y
  }
  
  /** Wykonuje ruch.
   * @param move		ruch.
   * @return			dane używane do cofania ruchu.
   */
  def unsafeMakeMove(move: Move): Option[Undo] = {
    val savedEnPassant = mEnPassant
    val savedHalfmoveClock = mHalfmoveClock
    val savedHashKey = mHashKey
    val undoData = move.moveType match {
      case MoveType.NormalMove | MoveType.Capture => unsafeMakeNormalMoveOrCapture(move)
      case MoveType.EnPassant                     => unsafeMakeEnPassant(move)
      case _                                      => unsafeMakeCastling(move)
    }
    if(undoData != -1) Some(Undo(move, undoData, savedEnPassant, savedHalfmoveClock, savedHashKey)) else None
  }

  /** Cofa ruch.
   * @param undo		dane cofania ruchu.
   */
  def unsafeUndoMove(undo: Undo): Unit = {
    undo.move.moveType match {
      case MoveType.NormalMove | MoveType.Capture => 
        unsafeUndoNormalMoveOrCaptureWithoutHashKey(undo.move, undo.data, undo.enPassant, undo.halfmoveClock)
      case MoveType.EnPassant                     =>
        unsafeUndoEnPassantWithoutHashKey(undo.move, undo.data, undo.enPassant, undo.halfmoveClock)
      case _                                      =>
        unsafeUndoCastlingWithoutHashKey(undo.move, undo.data, undo.enPassant, undo.halfmoveClock)
    }
    mHashKey = undo.hashKey
  }
  
  /** Sprawdza czy dane pole jest atakowane przez daną stronę.
   * @param sq			pole atakowane.
   * @param side		strona atakującia.
   * @return			jeśli strona atakuje to true.
   */
  def attack(sq: Int, side: Side): Boolean = {
    {
      val pawn = SidePieceOption.fromSideAndPiece(side, Piece.Pawn)
      Square.foldPawnCaptureSquares(sq, side.opposite)(false) { (b, _) => !b } { (_, src) => this(src) == pawn }
    } || {
      val knight = SidePieceOption.fromSideAndPiece(side, Piece.Knight)
      Square.foldNonSlidingMoveSquares(sq, Piece.Knight)(false) { (b, _) => !b } { (_, src) => this(src) == knight }
    } || {
      val king = SidePieceOption.fromSideAndPiece(side, Piece.King)
      Square.foldNonSlidingMoveSquares(sq, Piece.King)(false) { (b, _) => !b } { (_, src) => this(src) == king }
    } || {
      val bishop = SidePieceOption.fromSideAndPiece(side, Piece.Bishop)
      val queen = SidePieceOption.fromSideAndPiece(side, Piece.Queen)
      Square.foldSlidingMoveSquares(sq, Piece.Bishop)(false) { !_ } { _ => false } { (_, src) => this(src).isNone } { 
        (_, _) => false 
      } { 
        (_, src) => this(src) == bishop || this(src) == queen
      }
    } || {
      val rook = SidePieceOption.fromSideAndPiece(side, Piece.Rook)
      val queen = SidePieceOption.fromSideAndPiece(side, Piece.Queen)
      Square.foldSlidingMoveSquares(sq, Piece.Rook)(false) { !_ } { _ => false } { (_, src) => this(src).isNone } { 
        (_, _) => false 
      } { 
        (_, src) => this(src) == rook || this(src) == queen
      }
    }
  }
  
  /** Sprawdza czy jest szach dla danej strony.
   * @param side		strona.
   * @return			jeśli jest szach to true.
   */
  def sideInCheck(side: Side): Boolean =
    attack(mSList(StartSListIndexes(side.id)(Piece.King.id)), side.opposite)
  
  /** Sprawdza czy jest szach dla strony która ma ruch. */
  def inCheck: Boolean =
    attack(mSList(StartSListIndexes(side.id)(Piece.King.id)), side.opposite)
    
  /** Podobnie jak sideInCheck tylko nie używa cache */
  def sideInCheckNoCache(side: Side): Boolean =
    attack(mSList(StartSListIndexes(side.id)(Piece.King.id)), side.opposite)
    
  /** Podobnie jak inCheck tylko nie używa cache */
  def inCheckNoCache: Boolean =
    attack(mSList(StartSListIndexes(side.id)(Piece.King.id)), side.opposite)
}

/**
 * @author Łukasz Szpakowski
 */
object Board
{
  /** Tablica stron dla implementacji planszy. */
  private val Sides = Array(Side.White, Side.Black)

  /** Tablica indeksów początkowych dla danej bierki o określonej strony. */
  private val StartSListIndexes = Side.makeArray(
      Piece.makeArray(0, 8, 10, 12, 14, 16),
      Piece.makeArray(20, 28, 30, 32, 34, 36)
      )

  /** Tablica indeksów konczowych dla danej bierki o określonej strony. */  
  private val EndSListIndexes = Side.makeArray(
      Piece.makeArray(8, 10, 12, 14, 15, 17),
      Piece.makeArray(28, 30, 32, 34, 35, 37)
      )

  /** Tablica minimalnych indesków początkowych dla danej strony. */
  private val MinStartSListIndexes = Side.makeArray(0, 20)

  /** Tablica maksymalnych indesków konczowych dla danej strony. */
  private val MaxEndSListIndexes = Side.makeArray(17, 37)
  
  def apply(
      pieces: Seq[SidePieceOption],
      side: Side,
      castling: (Castling, Castling), 
      enPassant: SquareOption, 
      halfmoveClock: Int,
      fullmoveNumber: Int): Board =
    new Board(pieces, side, castling, enPassant, halfmoveClock, fullmoveNumber)
}