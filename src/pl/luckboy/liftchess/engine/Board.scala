package pl.luckboy.liftchess.engine

/** Klasa planszy. 
 * 
 * @author Łukasz Szpakowski
 */
class Board private(
    private val mPieces: Array[SidePieceOption],
    private var mSide: Side,
    castlingPair: (Castling, Castling),
    private var mEnPassant: SquareOption,
    private var mHalfmoveClock: Int,
    private var mFullmoveNumber: Int
    )
{
  import Board.Sides
  import Board.MinSListIndexes
  import Board.MaxSListIndexes
  
  /** Tablica dla roszad. Zawiera znaczniki roszad w odpowiednich pozycjach. */
  private val mCastlingArray = {
    val isWhiteKingsideCastling = ((castlingPair._1 & Castling.KingsideCastling) != Castling.NoneCastling)
    val isWhiteQueensideCastling = ((castlingPair._1 & Castling.QueensideCastling) != Castling.NoneCastling)
    val isBlackKingsideCastling = ((castlingPair._2 & Castling.KingsideCastling) != Castling.NoneCastling)
    val isBlackQueensideCastling = ((castlingPair._2 & Castling.QueensideCastling) != Castling.NoneCastling)

    Array(isBlackQueensideCastling, false, false, false, false, false, false, isBlackKingsideCastling) ++
    Array.fill(6 * 8)(false) ++
    Array(isWhiteQueensideCastling, false, false, false, false, false, false, isWhiteKingsideCastling)
  }

  /** Lista statyczna zawierająca pola poszczególnych bierek. */
  private val mSList = {
    val sList = Array.fill(40)(-1)
    val lastSListIndexes = Side.makeArray(
        MinSListIndexes(Side.White.id).clone(),
        MinSListIndexes(Side.Black.id).clone()
        )
    
    (0 to 63).foreach {
      sq => mPieces(sq).foldLeft(()) {
        (_, sp) =>
          val side = sp.side
          val piece = sp.piece
          val pieceId = if(lastSListIndexes(side.id)(piece.id) < MaxSListIndexes(side.id)(piece.id)) piece.id else Piece.Pawn.id
          require(lastSListIndexes(side.id)(pieceId) < MaxSListIndexes(side.id)(pieceId))
          sList(lastSListIndexes(side.id)(pieceId)) = sq
          lastSListIndexes(side.id)(pieceId) += 1
        }
    }
    sList
  }

  /** Indeksy do listy statycznej w postaci mapy (pole -> indeks do listy). */
  private val mSListIndexes = {
    val slistIndexes = Array.fill(64)(-1)

    (0 to 31).foreach { i => if(mSList(i) != -1) { slistIndexes(mSList(i)) = i } }
    slistIndexes
  }
  
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
        val j = MinSListIndexes(side.id)(piece.id)
        if(mSList(j) != -1) sum += 1
        if(mSList(j + 1) != -1) sum += 1
      }
      var i = MinSListIndexes(side.id)(Piece.Pawn.id)
      val n = MaxSListIndexes(side.id)(Piece.Pawn.id)
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
  def countPieces(piece: Piece): Int = 
    countSidePieces(Side.White, piece) + countSidePieces(Side.Black, piece)
  
  /** Podaje liczbę wszystkich bierek danej strony.
   * @param side		strona.
   * @return			liczba bierek.
   */
  def countAllSidePieces(side: Side): Int = {
    var sum = 0
    var i = MinSListIndexes(side.id)(Piece.Pawn.id)
    val n = MaxSListIndexes(side.id)(Piece.King.id)
    while(i < n) {
      if(mSList(i) != -1) sum += 1
      i += 1
    }
    sum
  }
  
  /** Podaje liczbę wszystkich bierek.
   * @return			liczba bierek.
   */
  def countAllPieces: Int =
    countAllSidePieces(Side.White) + countAllSidePieces(Side.Black)
  
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
      val sq = mSList(MinSListIndexes(side.id)(Piece.King.id))
      if(p(z, sq)) f(z, sq)  else z
    } else {
      var y = z
      if(piece != Piece.Pawn) {
        val j = MinSListIndexes(side.id)(piece.id)
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
      var i = MinSListIndexes(side.id)(Piece.Pawn.id)
      val n = MaxSListIndexes(side.id)(Piece.Pawn.id)
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
      val sq = mSList(MinSListIndexes(Side.White.id)(Piece.King.id))
      if(p(z, sq)) {
        val y = f(z, sq)
        val sq2 = mSList(MinSListIndexes(Side.Black.id)(Piece.King.id))
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
          var j = MinSListIndexes(side.id)(piece.id)
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
        var i = MinSListIndexes(side.id)(Piece.Pawn.id)
        val n = MaxSListIndexes(side.id)(Piece.Pawn.id)
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
    var i = MinSListIndexes(side.id)(Piece.Pawn.id)
    val n = MaxSListIndexes(side.id)(Piece.King.id)
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
      var i = MinSListIndexes(side.id)(Piece.Pawn.id)
      val n = MaxSListIndexes(side.id)(Piece.King.id)
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
    val res1 = if(mCastlingArray(Square(row, 0))) Castling.QueensideCastling else Castling.NoneCastling
    val res2 = res1 | (if(mCastlingArray(Square(row, 7))) Castling.KingsideCastling else Castling.NoneCastling)
    res2
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
  
  /** Słada następnik planszy. W rzeczywistości wykonuje ruch jeśli jest legalny i wykonuje daną funkcje. Po 
   * wykonaniu funkcji cofa ruch.
   * @param	move		ruch.
   * @param z			wartość początkowa.
   * @param f			funkcja składania.
   * @return			wynik funkcji lub wartość początkowa.
   */
  def unsafeFoldSuccessor[T](move: Move)(z: T)(f: (T, Board) => T): T = throw new Exception

  /** Słada następnik planszy dla ruchu pustego. W rzeczywistości  wykonuje pusty ruch jeśli jest legalny i 
   * wykonuje daną funkcje. Po wykonaniu funkcji cofa ruch pusty.
   * @param z			wartość początkowa.
   * @param f			funkcja.
   * @return			wynik funkcji lub wartość początkowa.
   */  
  def unsafeFoldNullSuccessor[T](z: T)(f: (T, Board) => T): T = throw new Exception
  
  /** Słada następnik planszy bez obliczania hash klucza. W rzeczywistości wykonuje ruch jeśli jest legalny i 
   * wykonuje daną funkcje. Po wykonaniu funkcji cofa ruch.
   * @param	move		ruch.
   * @param z			wartość początkowa.
   * @param f			funkcja składania.
   * @return			wynik funkcji lub wartość początkowa.
   */
  def unsafeFoldSuccessorWithoutHashKey[T](move: Move)(z: T)(f: (T, Board) => T): T = throw new Exception

  /** Słada następnik planszy dla ruchu pustego bez obliczania hash klucza. W rzeczywistości wykonuje pusty ruch 
   * jeśli jest legalnyi wykonuje daną funkcje. Po wykonaniu funkcji cofa ruch pusty.
   * @param z			wartość początkowa.
   * @param f			funkcja składania.
   * @return			wynik funkcji lub wartość początkowa.
   */  
  def unsafeFoldNullSuccessorWithoutHashKey[T](z: T)(f: (T, Board) => T): T = throw new Exception
  
  /** Wykonuje ruch.
   * @param move		ruch.
   * @return			dane używane do cofania ruchu.
   */
  def unsafeMakeMove(move: Move): Option[Undo] = throw new Exception

  /** Cofa ruch.
   * @param undo		dane cofania ruchu.
   */
  def unsafeUndoMove(undo: Undo): Unit = throw new Exception

  
  /** Sprawdza czy dane pole jest atakowane przez daną stronę.
   * @param sq			pole atakowane.
   * @param side		strona atakującia.
   * @return			jeśli strona atakuje to true.
   */
  def attack(sq: Int, side: Side): Boolean = throw new Exception
  
  /** Sprawdza czy jest szach dla danej strony.
   * @param side		strona.
   * @return			jeśli jest szach to true.
   */
  def sideInCheck(side: Side): Boolean = throw new Exception
  
  /** Sprawdza czy jest szach dla strony która ma ruch. */
  def inCheck: Boolean = throw new Exception
}

/**
 * @author Łukasz Szpakowski
 */
object Board
{
  /** Tablica stron dla implementacji planszy. */
  private val Sides = Array(Side.White, Side.Black)

  /** Tablica minimalnych indeksów dla danej bierki o określonej strony. */
  private val MinSListIndexes = Side.makeArray(
      Piece.makeArray(0, 8, 10, 12, 14, 16),
      Piece.makeArray(20, 28, 30, 32, 34, 36)
      )

  /** Tablica maksymalnych indeksów dla danej bierki o określonej strony. */  
  private val MaxSListIndexes = Side.makeArray(
      Piece.makeArray(8, 10, 12, 14, 15, 17),
      Piece.makeArray(28, 30, 32, 34, 35, 37)
      )
  
  def apply(
      pieces: Seq[SidePieceOption],
      side: Side,
      castling: (Castling, Castling), 
      enPassant: SquareOption, 
      halfmoveClock: Int,
      fullmoveNumber: Int): Board =
    new Board(pieces.toArray, side, castling, enPassant, halfmoveClock, fullmoveNumber)
}