package pl.luckboy.liftchess.engine

/** A board class. 
 * 
 * @author Łukasz Szpakowski
 */
@cloneable
final class Board private(
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
  
  // Checks whether en passant is correct.
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

  // Checks whether castling is correct.
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
  
  /** The piece array. */
  protected val mPieces = pieces.toArray
  
  /** The castling array. This array contains castling marks. */
  protected val mCastlingArray = (
      Array(castlingPair._2 & Castling.QueensideCastling) ++
      Array.fill(6)(Castling.NoneCastling) ++
      Array(castlingPair._2 & Castling.KingsideCastling) ++ 
      Array.fill(6 * 8)(Castling.NoneCastling) ++
      Array(castlingPair._1 & Castling.QueensideCastling) ++
      Array.fill(6)(Castling.NoneCastling) ++
      Array(castlingPair._1 & Castling.KingsideCastling)
      )

  /** The list contains squares for pieces. */
  protected val mSList = {
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

  /** The indexes of static list as map (square -> index of list). */
  protected val mSListIndexes = {
    val slistIndexes = Array.fill(64)(-1)

    (0 to 39).foreach { i => if(mSList(i) != -1) { slistIndexes(mSList(i)) = i } }
    slistIndexes
  }  
  
  /** The hash key. */
  protected var mHashKey = (
      (0 to 63).foldLeft(0L) { 
        (key, sq) => 
          mPieces(sq).foldLeft(key) { 
            (key, sidePiece) => key ^ Zobrist.pieceSquareKey(sq, sidePiece.side, sidePiece.piece)
          }
      } ^ 
      Zobrist.sideKey(side) ^ 
      Zobrist.castlingKey(Side.White, castling(Side.White)) ^
      Zobrist.castlingKey(Side.Black, castling(Side.Black)) ^
      Zobrist.enPassantKey(mEnPassant)
  )
  
  /** Returns number of occurrences of specified piece for specified side.
   * @param side		the side.
   * @param piece		the piece.
   * @return			the number of pieces.
   */
  def countSidePieces(side: Side, piece: Piece): Int = {
    if(piece eq Piece.King) {
      1
    } else {
      var sum = 0
      if(piece ne Piece.Pawn) {
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

  /** Returns number of occurrences of piece for any side.
   * @param piece		the piece.
   * @return			the number of pieces.
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
  
  /** Returns number of all pieces for specified side.
   * @param side 		the side.
   * @return			the number of pieces.
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
  
  /** Returns number of all pieces for any side.
   * @return			the number of pieces.
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

  /** Returns number of empty squares.
   * @return			the number of empty squares.
   */
  def countEmptySquares: Int =
    64 - countAllPieces

  /** Folds occurrences of specified piece for specified side.
   * @param side 		the side.
   * @param piece		the piece.
   * @param	z			the start value.
   * @param p			the stopping function (if this function returns false, there stops folding).
   * @param f			the folding function.
   * @return			the folding result.
   */
  @inline
  def foldSidePieces[@specialized T](side: Side, piece: Piece)(z: T)(p: (T, Int) => Boolean)(f: (T, Int) => T): T = {
    if(piece eq Piece.King) {
      val sq = mSList(StartSListIndexes(side.id)(Piece.King.id))
      if(p(z, sq)) f(z, sq)  else z
    } else {
      var y = z
      if(piece ne Piece.Pawn) {
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
  
  /** Folds occurrences of specified piece for any side.
   * @param piece		the piece.
   * @param	z			the start value.
   * @param p			the stopping function (if this function returns false, there stops folding).
   * @param f			the folding function.
   * @return			the folding result.
   */
  @inline
  def foldPieces[@specialized T](piece: Piece)(z: T)(p: (T, Int) => Boolean)(f: (T, Int) => T): T = {
    if(piece eq Piece.King) {
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
        if(piece ne Piece.Pawn) {
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
  
  /** Folds all piece for specified side.
   * @param side		the side.
   * @param	z			the start value.
   * @param p			the stopping function (if this function returns false, there stops folding).
   * @param f			the folding function.
   * @return			the folding result.
   */
  @inline
  def foldAllSidePieces[@specialized T](side: Side)(z: T)(p: (T, Int) => Boolean)(f: (T, Int) => T): T = {
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

  /** Folds all piece for any side.
   * @param	z			the start value.
   * @param p			the stopping function (if this function returns false, there stops folding).
   * @param f			the folding function.
   * @return			the folding result.
   */
  @inline
  def foldAllPieces[@specialized T](z: T)(p: (T, Int) => Boolean)(f: (T, Int) => T): T = {
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

  /** Folds empty squares.
   * @param	z			the start value.
   * @param p			the stopping function (if this function returns false, there stops folding).
   * @param f			the folding function.
   * @return			the folding result.
   */
  @inline
  def foldEmptySquares[@specialized T](z: T)(p: (T, Int) => Boolean)(f: (T, Int) => T): T = {
    var y = z
    var sq = 0
    while(sq < 64) {
      if(this(sq) eq SidePieceOption.None) {
        if(!p(y, sq)) return y
        y = f(y, sq)
      }
      sq += 1
    }
    y
  }

  /** The side. */
  def side: Side =
    mSide

  /** The available castling for specified side.
   * @param side		the side.
   */
  def castling(side: Side): Castling = {
    val row = if(side == Side.White) 7 else 0
    mCastlingArray(Square(row, 0)) | mCastlingArray(Square(row, 7))
  }

  /** The square for en passant. */
  def enPassant: SquareOption =
    mEnPassant

  /** The number of half move. */
  def halfmoveClock: Int =
    mHalfmoveClock

  /** The number of moves. */ 
  def fullmoveNumber: Int =
    mFullmoveNumber
  
  /** The hash key. */
  def hashKey: Long =
    mHashKey

  /** Returns piece at specified square. */
  def apply(sq: Int): SidePieceOption =
    mPieces(sq)
  
  /** This method is like unsafeMakeNormalMoveOrCaptureWithoutHashKey but this method calculates hash key.
   * @param move		the move.
   * @return			the date used to undo move or -1 if doesn't make move. 
   */
  protected def unsafeMakeNormalMoveOrCapture(move: Move) = {
    val savedDstPiece = this(move.destination)
    val savedWhiteCastling = mCastlingArray(Square(7, 0)) | mCastlingArray(Square(7, 7))
    val savedBlackCastling = mCastlingArray(Square(0, 0)) | mCastlingArray(Square(0, 7))
    val savedEnPassant = mEnPassant
    val undo = unsafeMakeNormalMoveOrCaptureWithoutHashKey(move)
    if(undo != -1) {
      val piece = this(move.destination)
      // Removes piece from source and stuff.
      mHashKey ^= Zobrist.pieceSquareKey(move.source, side.opposite, move.piece)
      mHashKey ^= Zobrist.castlingKey(Side.White, savedWhiteCastling)
      mHashKey ^= Zobrist.castlingKey(Side.Black, savedBlackCastling)
      mHashKey ^= Zobrist.enPassantKey(savedEnPassant)
      mHashKey ^= Zobrist.sideKey(side.opposite)
      // Removes captured piece.
      mHashKey ^= savedDstPiece.foldLeft(0L) {
        (_, sidePiece) => Zobrist.pieceSquareKey(move.destination, sidePiece.side, sidePiece.piece)
      }
      // Sets piece on destination and stuff.
      mHashKey ^= Zobrist.pieceSquareKey(move.destination, side.opposite, move.promotionPiece.foldLeft(move.piece) { (_, promPiece) => promPiece })
      mHashKey ^= Zobrist.castlingKey(Side.White, mCastlingArray(Square(7, 0)) | mCastlingArray(Square(7, 7)))
      mHashKey ^= Zobrist.castlingKey(Side.Black, mCastlingArray(Square(0, 0)) | mCastlingArray(Square(0, 7)))
      mHashKey ^= Zobrist.enPassantKey(mEnPassant)
      mHashKey ^= Zobrist.sideKey(side)
    }
    undo
  }
     
  /** This method is like unsafeMakeEnPassantWithoutHashKey but this method calculates hash key.
   * @param move		the move.
   * @return			the date used to undo move or -1 if doesn't make move. 
   */
  protected def unsafeMakeEnPassant(move: Move) = {
    val savedEnPassant = mEnPassant
    val undo = unsafeMakeEnPassantWithoutHashKey(move)
    if(undo != -1) {
      val capSq = Square(Square.toRow(move.source), Square.toColumn(move.destination))
      // Removes pawn from source and stuff.      
      mHashKey ^= Zobrist.pieceSquareKey(move.source, side.opposite, Piece.Pawn)
      mHashKey ^= Zobrist.enPassantKey(savedEnPassant)
      mHashKey ^= Zobrist.sideKey(side.opposite)
      // Removes captured pawn.
      mHashKey ^= Zobrist.pieceSquareKey(capSq, side, Piece.Pawn)
      // Sets pawn on destination and stuff.
      mHashKey ^= Zobrist.pieceSquareKey(move.destination, side.opposite, Piece.Pawn)
      mHashKey ^= Zobrist.enPassantKey(SquareOption.None)
      mHashKey ^= Zobrist.sideKey(side)
    }
    undo
  }

  /** This method is like unsafeMakeCastlingWithoutHashKey but this method calculates hash key.
   * @param move		the move.
   * @return			the date used to undo move or -1 if doesn't make move. 
   */
  protected def unsafeMakeCastling(move: Move) = {
    val row = if(side eq Side.White) 7 else 0
    val savedEnPassant = mEnPassant
    val savedCastling = mCastlingArray(Square(row, 0)) | mCastlingArray(Square(row, 7))
    val undo = unsafeMakeCastlingWithoutHashKey(move)
    if(undo != -1) {
      val oppSide = side.opposite
      val kingSrc = Square(row, move.source)
      val kingDst = Square(row, move.destination)
      val rookSrc = Square(row, if(move.moveType eq MoveType.KingsideCastling) 7 else 0)
      val rookDst = Square(row, if(move.moveType eq MoveType.KingsideCastling) 5 else 3)
      mHashKey ^= Zobrist.sideKey(oppSide)
      // Removes king and rook from sources and stuff.
      mHashKey ^= Zobrist.pieceSquareKey(kingSrc, side.opposite, Piece.King)
      mHashKey ^= Zobrist.pieceSquareKey(rookSrc, side.opposite, Piece.Rook)
      mHashKey ^= Zobrist.castlingKey(side.opposite, savedCastling)
      mHashKey ^= Zobrist.enPassantKey(savedEnPassant)
      // Sets king and rook on destinations and stuff.
      mHashKey ^= Zobrist.pieceSquareKey(kingDst, side.opposite, Piece.King)
      mHashKey ^= Zobrist.pieceSquareKey(rookDst, side.opposite, Piece.Rook)
      mHashKey ^= Zobrist.castlingKey(side.opposite, Castling.NoneCastling)
      mHashKey ^= Zobrist.enPassantKey(SquareOption.None)
      mHashKey ^= Zobrist.sideKey(side)
    }
    undo
  }

  /** Folds successor for board. Really, makes move if move is legal and, then evaluates function and, then undo move.
   * @param move		the move.
   * @param	z			the start value.
   * @param f			the folding function.
   * @return			the result of function or start value.
   */
  @inline
  def unsafeFoldSuccessor[@specialized T](move: Move)(z: T)(f: (T, Board) => T): T = {
    val savedEnPassant = mEnPassant
    val savedHalfmoveClock = mHalfmoveClock
    val savedHashKey = mHashKey
    move.moveType match {
      case MoveType.NormalMove | MoveType.Capture =>
      	val undo = unsafeMakeNormalMoveOrCapture(move)
      	if(undo != -1) {
      	  try {
      	    f(z, this)
      	  } finally {
      	    unsafeUndoNormalMoveOrCaptureWithoutHashKey(move, undo, savedEnPassant, savedHalfmoveClock)
      	    mHashKey = savedHashKey
      	  }
      	} else {
      	  z
      	}
      case MoveType.EnPassant                     =>
      	val undo = unsafeMakeEnPassant(move)
      	if(undo != -1) {
      	  try {
      	    f(z, this)
      	  } finally {
      	    unsafeUndoEnPassantWithoutHashKey(move, undo, savedEnPassant, savedHalfmoveClock)
      	    mHashKey = savedHashKey
      	  }
      	} else {
      	  z
      	}
      case _                                      =>
      	val undo = unsafeMakeCastling(move)
      	if(undo != -1) {
      	  try {
      	    f(z, this)
      	  } finally {
      	    unsafeUndoCastlingWithoutHashKey(move, undo, savedEnPassant, savedHalfmoveClock)
      	    mHashKey = savedHashKey
      	  }
      	} else {
      	  z
      	}
    }
  }

  /** Folds successor for null move. Really makes move and, then evaluates function and, then undo move.
   * @param z			the start value
   * @param f			the folding function.
   * @return			the result of function or start value.
   */
  @inline
  def unsafeFoldNullSuccessor[@specialized T](z: T)(f: (T, Board) => T): T = {
    val savedHashKey = mHashKey
    mSide = side.opposite
    mHashKey ^= Zobrist.sideKey(mSide.opposite)
    mHashKey ^= Zobrist.sideKey(mSide)
    try {
      f(z, this)
    } finally {
      mSide = side.opposite
      mHashKey = mHashKey
    }
  }

  /** Makes normal move or capture (it haven't to been en pasant or castling) but this method doesn't calculate hash key.
   * @param move		the move.
   * @return			the date used to undo move or -1 if doesn't make move. 
   */
  protected def unsafeMakeNormalMoveOrCaptureWithoutHashKey(move: Move) = {
    val piece = move.piece
    val src = move.source
    val dst = move.destination
    val savedDstPiece = mPieces(dst)
    val savedDstSListIndex = mSListIndexes(dst)
    
    // Bierki.
    mPieces(dst) = SidePieceOption.fromSideAndPiece(side, move.promotionPiece.foldLeft(piece) { (_, promPiece) => promPiece })
    mPieces(src) = SidePieceOption.None
    // Lista.
    if(mSListIndexes(dst) != -1) mSList(mSListIndexes(dst)) = -1
    mSList(mSListIndexes(src)) = dst
    mSListIndexes(dst) = mSListIndexes(src)
    mSListIndexes(src) = -1
    if(!inCheckNoCache) {
      val savedWhiteCastling = mCastlingArray(Square(7, 0)) | mCastlingArray(Square(7, 7))
      val savedBlackCastling = mCastlingArray(Square(0, 0)) | mCastlingArray(Square(0, 7))
      
      // Roszady.
      mCastlingArray(dst) = Castling.NoneCastling
      mCastlingArray(src) = Castling.NoneCastling
      piece match {
        case Piece.Pawn =>
          // Ustawia dla pola en passant.
          val dstRow = if(side eq Side.White) 4 else 3
          val col = Square.toColumn(dst)
          val enpSq = Square(if(side eq Side.White) 5 else 2, col)
          val pawn = SidePieceOption.fromSideAndPiece(side.opposite, Piece.Pawn) 
          
          if(Square.toRow(dst) == dstRow && Square.foldPawnCaptureSquares(enpSq, side)(false) { (b, _) => !b } { (_, sq) => this(sq) eq pawn } ) {
        	mEnPassant = SquareOption(enpSq)
          } else {
            mEnPassant = SquareOption.None
          }
          // Wzerowanie liczby półruchów.
          mHalfmoveClock = 0
        case Piece.King =>
          val row = if(side eq Side.White) 7 else 0
          // Ustawia roszady (usuwa je).
          mCastlingArray(Square(row, 0)) = Castling.NoneCastling
          mCastlingArray(Square(row, 7)) = Castling.NoneCastling
          // Ustawia dla pola en passant.
          mEnPassant = SquareOption.None
          // Zwiękrzenie liczby półruchów.
          mHalfmoveClock = if(savedDstPiece eq SidePieceOption.None) mHalfmoveClock + 1 else 0
        case _          =>
          // Ustawia dla pola en passant.
          mEnPassant = SquareOption.None
          // Zwiękrzenie liczby półruchów.
          mHalfmoveClock = if(savedDstPiece eq SidePieceOption.None) mHalfmoveClock + 1 else 0
      }
      // Liczba ruchów.
      if(side eq Side.Black) mFullmoveNumber += 1
      // Strona.
      mSide = mSide.opposite
      savedWhiteCastling.id | (savedBlackCastling.id << 4) | ((savedDstSListIndex & 255) << 8) | (savedDstPiece.id << 16)
    } else {
      // Lista.
      mSListIndexes(src) = mSListIndexes(dst)
      mSListIndexes(dst) = savedDstSListIndex
      mSList(mSListIndexes(src)) = src
      if(mSListIndexes(dst) != -1) mSList(mSListIndexes(dst)) = dst
      // Bierki.
      mPieces(src) = SidePieceOption.fromSideAndPiece(side, piece)
      mPieces(dst) = savedDstPiece
      -1
    }
  }

  /** Undoes normal move or capture (it haven't to been en passant or castling) but this method doesn't calculate hash key.
   * @param move				the move.
   * @param undo				the data for undo move.
   * @param savedEnPassant		the saved square for en passant.
   * @param savedHalfmoveClock	the saved number of half move from last capture or pawn move.
   */
  protected def unsafeUndoNormalMoveOrCaptureWithoutHashKey(move: Move, undo: Int, savedEnPassant: SquareOption, savedHalfmoveClock: Int) = {
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
    if(side eq Side.Black) mFullmoveNumber -= 1
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

  /** Makes en passant but this method doesn't hash key.
   * @param move		the move.
   * @return			the date used to undo move or -1 if doesn't make move. 
   */
  protected def unsafeMakeEnPassantWithoutHashKey(move: Move) = {
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
      if(side eq Side.Black) mFullmoveNumber += 1
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

  /** Undoes en passant but this method doesn't calculate hash key.
   * @param move 				the move.
   * @param	undo				the data for undo move.
   * @param savedEnPassant		the saved square for en passant.
   * @param savedHalfmoveClock	the saved number of half move from last capture or pawn move.
   */
  protected def unsafeUndoEnPassantWithoutHashKey(move: Move, undo: Int, savedEnPassant: SquareOption, savedHalfmoveClock: Int) = {
    val src = move.source
    val dst = move.destination
    val capSq = Square(Square.toRow(src), Square.toColumn(dst))
    val savedCapSListIndex = (undo << 24) >> 24

    // Strona.
    mSide = mSide.opposite
    // Liczba ruchów i półruchów.
    if(side eq Side.Black) mFullmoveNumber -= 1
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

  /** Makes castling but this method doesn't hash key.
   * @param move		the move.
   * @return			the date used to undo move or -1 if doesn't make move. 
   */
  protected def unsafeMakeCastlingWithoutHashKey(move: Move) = {
    val row = if(side == Side.White) 7 else 0
    val kingSrc = Square(row, move.source)
    val kingDst = Square(row, move.destination)
    val rookSrc = Square(row, if(move.moveType eq MoveType.KingsideCastling) 7 else 0)
    val rookDst = Square(row, if(move.moveType eq MoveType.KingsideCastling) 5 else 3)
    
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
      if(side eq Side.Black) mFullmoveNumber += 1
      // Strona.
      mSide = mSide.opposite
      savedCastling.id
    } else {
      -1
    }
  }
  
  /** Undoes castling but this method doesn't calculate hash key.
   * @param move 				the move.
   * @param	undo				the data for undo move.
   * @param savedEnPassant		the saved square for en passant.
   * @param savedHalfmoveClock	the saved number of half move from last capture or pawn move.
   */
  protected def unsafeUndoCastlingWithoutHashKey(move: Move, undo: Int, savedEnPassant: SquareOption, savedHalfmoveClock: Int) = {
    val row = if(side == Side.Black) 7 else 0
    val kingSrc = Square(row, move.source)
    val kingDst = Square(row, move.destination)
    val rookSrc = Square(row, if(move.moveType eq MoveType.KingsideCastling) 7 else 0)
    val rookDst = Square(row, if(move.moveType eq MoveType.KingsideCastling) 5 else 3)
    val savedCastling = Castling(undo)

    // Strona.
    mSide = mSide.opposite
    // Liczba ruchów oraz półruchów.
    if(side eq Side.Black) mFullmoveNumber -= 1
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
  
  /** Folds successor for board but this method doesn't calculate hash key. Really makes move if move is legal and, then 
   * evaluates function and, then undo move.
   * @param move		the move.
   * @param	z			the start value.
   * @param f			the folding function.
   * @return			the result of function or start value.
   */
  @inline
  def unsafeFoldSuccessorWithoutHashKey[@specialized T](move: Move)(z: T)(f: (T, Board) => T): T = {
    val savedEnPassant = mEnPassant
    val savedHalfmoveClock = mHalfmoveClock
    move.moveType match {
      case MoveType.NormalMove | MoveType.Capture =>
        val undo = unsafeMakeNormalMoveOrCaptureWithoutHashKey(move)
        if(undo != -1) {
          try {
            f(z, this)
          } finally {
            unsafeUndoNormalMoveOrCaptureWithoutHashKey(move, undo, savedEnPassant, savedHalfmoveClock)
          }
        } else {
          z
        }
      case MoveType.EnPassant                     =>
        val undo = unsafeMakeEnPassantWithoutHashKey(move)
        if(undo != -1) {
          try {
            f(z, this)
          } finally {
            unsafeUndoEnPassantWithoutHashKey(move, undo, savedEnPassant, savedHalfmoveClock)
          }
        } else {
          z
        }
      case _                                      =>
        val undo = unsafeMakeCastlingWithoutHashKey(move)
        if(undo != -1) {
          try {
            f(z, this)
          } finally {
            unsafeUndoCastlingWithoutHashKey(move, undo, savedEnPassant, savedHalfmoveClock)
          }
        } else {
          z
        }
    }
  }

  /** Folds successor for null move but this method doesn't calculate hash key. Really makes move and, then evaluates 
   * function and, then undo move.
   * @param z			the start value
   * @param f			the folding function.
   * @return			the result of function or start value.
   */
  @inline
  def unsafeFoldNullSuccessorWithoutHashKey[@specialized T](z: T)(f: (T, Board) => T): T = {
    mSide = mSide.opposite
    try {
      f(z, this)
    } finally {
      mSide = mSide.opposite
    }
  }

  /** Makes move.
   * @param move		the move.
   * @return			the data used to undo move.
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

  /** Undoes move.
   * @param undo		the data for undo move.
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
  
  /** Checks whether specified square be attack by specified side.
   * @param sq			the square.
   * @param side		the side.
   * @return			true if side attack.
   */
  def attack(sq: Int, side: Side): Boolean = {
    {
      val pawn = SidePieceOption.fromSideAndPiece(side, Piece.Pawn)
      Square.foldPawnCaptureSquares(sq, side.opposite)(false) { (b, _) => !b } { (_, src) => this(src) eq pawn }
    } || {
      val knight = SidePieceOption.fromSideAndPiece(side, Piece.Knight)
      Square.foldNonSlidingMoveSquares(sq, Piece.Knight)(false) { (b, _) => !b } { (_, src) => this(src) eq knight }
    } || {
      val king = SidePieceOption.fromSideAndPiece(side, Piece.King)
      Square.foldNonSlidingMoveSquares(sq, Piece.King)(false) { (b, _) => !b } { (_, src) => this(src) eq king }
    } || {
      val bishop = SidePieceOption.fromSideAndPiece(side, Piece.Bishop)
      val queen = SidePieceOption.fromSideAndPiece(side, Piece.Queen)
      Square.foldSlidingMoveSquares(sq, Piece.Bishop)(false) { !_ } { _ => false } { (_, src) => this(src).isNone } { 
        (_, _) => false 
      } { 
        (_, src) => (this(src) eq bishop) || (this(src) eq queen)
      }
    } || {
      val rook = SidePieceOption.fromSideAndPiece(side, Piece.Rook)
      val queen = SidePieceOption.fromSideAndPiece(side, Piece.Queen)
      Square.foldSlidingMoveSquares(sq, Piece.Rook)(false) { !_ } { _ => false } { (_, src) => this(src).isNone } { 
        (_, _) => false 
      } { 
        (_, src) => (this(src) eq rook) || (this(src) eq queen)
      }
    }
  }

  /** Checks whether specified side is in check.
   * @param side		the side.
   * @return			true if side is in check.
   */
  def sideInCheck(side: Side): Boolean =
    attack(mSList(StartSListIndexes(side.id)(Piece.King.id)), side.opposite)

  /** Checks whether side that have move is in check. */
  def inCheck: Boolean =
    attack(mSList(StartSListIndexes(side.id)(Piece.King.id)), side.opposite)

  /** The method is like sideInCheck but this method doesn't use cache. */
  def sideInCheckNoCache(side: Side): Boolean =
    attack(mSList(StartSListIndexes(side.id)(Piece.King.id)), side.opposite)
    
  /** The method is like inCheck but this method doesn't use cache. */
  def inCheckNoCache: Boolean =
    attack(mSList(StartSListIndexes(side.id)(Piece.King.id)), side.opposite)
    
  override def equals(that: Any): Boolean =
    that match {
      case bd: Board => 
        (0 to 63).forall { sq => this(sq) == bd(sq) } &&
        side == bd.side &&
        castling(Side.White) == castling(Side.Black) &&
        enPassant == bd.enPassant &&
        halfmoveClock == bd.halfmoveClock &&
        fullmoveNumber == bd.fullmoveNumber
      case _        =>
        false
    }

  override def hashCode: Int =
    (mPieces.toSeq, side.hashCode, castling(Side.White), castling(Side.Black), enPassant, halfmoveClock, fullmoveNumber).hashCode
  
  override def clone(): Board =
    Board(mPieces.toSeq, side, (castling(Side.White), castling(Side.Black)), enPassant, halfmoveClock, fullmoveNumber)
}

/**
 * @author Łukasz Szpakowski
 */
object Board
{
  /** The array of sides for board implementation. */ 
  private val Sides = Array(Side.White, Side.Black)

  /** The array of start indexes of specified piece for specified side. */
  private val StartSListIndexes = Side.makeArray(
      Piece.makeArray(0, 8, 10, 12, 14, 16),
      Piece.makeArray(20, 28, 30, 32, 34, 36)
      )

  /** The array of end indexes of specified piece for specified side. */
  private val EndSListIndexes = Side.makeArray(
      Piece.makeArray(8, 10, 12, 14, 15, 17),
      Piece.makeArray(28, 30, 32, 34, 35, 37)
      )

  /** The array of minimal indexes of specified side. */
  private val MinStartSListIndexes = Side.makeArray(0, 20)

  /** The array of maximal indexes of specified side. */
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