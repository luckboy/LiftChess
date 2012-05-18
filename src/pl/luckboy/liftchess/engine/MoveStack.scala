package pl.luckboy.liftchess.engine

/** A class for move stack.
 * 
 * @author Łukasz Szpakowski
 */
final class MoveStack(maxDepth: Int, maxMoves: Int)
{
  /** The stack pointer for the indexes. */
  protected var mSp = 0
  
  /** The index stack. */
  protected val mStack = new Array[Int](maxDepth + 1)
  
  /** The start index for currently generated moves. */
  protected var mStartMoveIndex = 0

  /** The end index for currently generated moves. */
  protected var mEndMoveIndex = 0

  /** The move array with score. */
  protected val mMoves = new Array[Int](maxMoves * 2)
  
  mStack(0) = 0

  /** Pushes the move into the stack.
   * @param piece		the piece.
   * @param src			the move source.
   * @param dst			the move destination.
   * @param promPiece	the piece that will be promotes by the pawn.
   * @param moveType	the move type.
   */
  private def pushMove(piece: Piece, src: Int, dst: Int, promPiece: PieceOption, moveType: MoveType) = {
    mMoves(mEndMoveIndex) = piece.id | (promPiece.id << 4) | (src << 8) | (dst << 16) | (moveType.id << 24) 
    mEndMoveIndex += 2
  }

  private def generatePseudoLegalEnPassantsTo(bd: Board, dst: Int) = {
    val side = bd.side
    val pawn = SidePieceOption.fromSideAndPiece(side, Piece.Pawn)
    Square.foldPawnCaptureSquares(dst, side.opposite)(()) { (_, _) => true } {
      (_, src) => if(bd(src) eq pawn) pushMove(Piece.Pawn, src, dst, PieceOption.None, MoveType.EnPassant)
    }
  }
  
  /** Generates a pseudo legal en passants and, then pushes they into the stack.
   * @param bd			the board.
   */
  private def generatePseudoLegalEnPassants(bd: Board) =
    bd.enPassant.foldLeft(()) { (_, dst) => generatePseudoLegalEnPassantsTo(bd, dst) }

  /** Generates a pseudo legal promotions and, then pushes they into the stack.
   * @param bd			the board.
   * @param src			the move source.
   */
  private def generatePseudoLegalPromotionsFrom(bd: Board, src: Int) =
    Square.foldMoveSquares(src, bd.side, Piece.Pawn)(()) { (_, dst) => bd(dst).isNone } {
      (_, dst) => 
        pushMove(Piece.Pawn, src, dst, PieceOption.Queen, MoveType.NormalMove)
        pushMove(Piece.Pawn, src, dst, PieceOption.Knight, MoveType.NormalMove)
    } {
      (_, dst) =>
        if(!bd(dst).isSide(bd.side)) {
          pushMove(Piece.Pawn, src, dst, PieceOption.Queen, MoveType.Capture)
          pushMove(Piece.Pawn, src, dst, PieceOption.Knight, MoveType.Capture)
        }
    }
  
  /** Generates a pseudo legal normal move and captures and, then pushes they into the stack.
   * @param bd			the board.
   * @param src			the source move.
   * @param piece		the piece.
   */
  private def generatePseudoLegalNormalMovesAndCapturesFrom(bd: Board, src: Int, piece: Piece) = {
    Square.foldMoveSquares(src, bd.side, piece)(()) { (_, dst) => bd(dst).isNone } {
      (_, dst) => pushMove(piece, src, dst, PieceOption.None, MoveType.NormalMove)
    } {
      (_, dst) => if(!bd(dst).isSide(bd.side)) pushMove(piece, src, dst, PieceOption.None, MoveType.Capture)
    }
  }

  private def generatePseudoLegalMovesFrom(bd: Board, src: Int, promSrcRow: Int) = {
    bd(src).foldPiece(()) {
      (_, piece) => 
        if((piece eq Piece.Pawn) && Square.toRow(src) == promSrcRow)
          // Promocje.
          generatePseudoLegalPromotionsFrom(bd, src)
        else
          // Nie promocje.
          generatePseudoLegalNormalMovesAndCapturesFrom(bd, src, piece)
    }
  }
  
  /** Generates a pseudo legal moves and pushes they into the stack.
   * @param bd			the board.
   */
  def generatePseudoLegalMoves(bd: Board): Unit = {
    startPushMoves()
    val side = bd.side
    
    // Roszada krótka.
    val row1 = if(side eq Side.White) 7 else 0
    if((bd.castling(side) & Castling.KingsideCastling) ne Castling.NoneCastling) 
      if((bd(Square(row1, 5)) eq SidePieceOption.None) && (bd(Square(row1, 6)) eq SidePieceOption.None))
        pushMove(Piece.King, 4, 6, PieceOption.None, MoveType.KingsideCastling)
    // Roszada długa.
    if((bd.castling(side) & Castling.QueensideCastling) ne Castling.NoneCastling)
      if((bd(Square(row1, 1)) eq SidePieceOption.None) && (bd(Square(row1, 2)) eq SidePieceOption.None) && (bd(Square(row1, 3)) eq SidePieceOption.None))
        pushMove(Piece.King, 4, 2, PieceOption.None, MoveType.QueensideCastling)
    // Inne ruchy.
    val promSrcRow = if(side eq Side.White) 1 else 6
    bd.foldAllSidePieces(side)(()) { (_, _) => true } { 
      (_, src) => generatePseudoLegalMovesFrom(bd, src, promSrcRow)
    }
    // Bicia w przelocie.
    generatePseudoLegalEnPassants(bd)
  }

  /** Generates a pseudo legal captures and, then pushes they into the the stack.
   * @param bd			the board.
   * @param src			the source move.
   * @param piece		the piece.
   */
  private def generatePseudoLegalCapturesFrom(bd: Board, src: Int, piece: Piece) = {
    Square.foldMoveSquares(src, bd.side, piece)(()) { (_, dst) => bd(dst).isNone } { (_, _) => () } {
      (_, dst) => if(!bd(dst).isSide(bd.side)) pushMove(piece, src, dst, PieceOption.None, MoveType.Capture)
    }
  }

  private def generatePseudoLegalGoodMovesFrom(bd: Board, src: Int, promSrcRow: Int) = {
    bd(src).foldPiece(()) {
      (_, piece) => 
        if((piece eq Piece.Pawn) && Square.toRow(src) == promSrcRow)
          // Promocje.
          generatePseudoLegalPromotionsFrom(bd, src)
        else
          // Bicia
          generatePseudoLegalCapturesFrom(bd, src, piece)
    }
  }
  
  /** Generates a pseudo legal moves those may be potentially good and, then pushes they into stack.
   * @param bd			the board.
   */
  def generatePseudoLegalGoodMoves(bd: Board): Unit = {
    startPushMoves()
    val side = bd.side

    // Inne ruchy.
    val promSrcRow = if(side eq Side.White) 1 else 6
    bd.foldAllSidePieces(side)(()) { (_, _) => true } {
      (_, src) => generatePseudoLegalGoodMovesFrom(bd, src, promSrcRow)
    }
    // Bicia w przelocie.
    generatePseudoLegalEnPassants(bd)
  }

  /** Begin to push new moves. This method uses in the move generators */
  private def startPushMoves() = {
    mSp += 1
    mStack(mSp) = mEndMoveIndex
    mStartMoveIndex = mEndMoveIndex
  }

  /** Pops the moves from stack. */
  def popMoves(): Unit = {
    mEndMoveIndex = mStartMoveIndex
    mSp -= 1
    mStartMoveIndex = mStack(mSp)
  }
  
  /** Pushes a pseudo legal moves and, then evaluates the function and, then pops the moves from stack. 
   * @param bd			the board.
   * @param f			the function.
   * @return			the result of function.
   */
  @inline
  def generatePseudoLegalMovesWithPopMoves[@specialized T](bd: Board)(f: => T): T = {
    generatePseudoLegalMoves(bd)
    try { f } finally { popMoves() }
  }

  /** Pushes a pseudo legal moves those may be potentially good and, then evaluates the function, and then pops the moves 
   * from stack.
   * @param bd			the board.
   * @param f			the function.
   * @return			the result of function.
   */
  @inline
  def generatePseudoLegalGoodMovesWithPopMoves[@specialized T](bd: Board)(f: => T): T = {
    generatePseudoLegalGoodMoves(bd)
    try { f } finally { popMoves() }
  }

  /** The number of moves */
  def size: Int =
    (mEndMoveIndex - mStartMoveIndex) >> 1

  /** Returns the move at specified index.
   * @param i			the index.
   * @return			the move.
   */
  def move(i: Int): Move = {
    val data = mMoves(mStartMoveIndex + (i << 1))
    Move(Piece(data & 15), (data >> 8) & 255, (data >> 16) & 255, PieceOption((data >> 4) & 15), MoveType(data >> 24))
  }

  /** Returns the move score at specified index.
   * @param	i			the index.
   * @return			the move.
   */
  def score(i: Int): Int =
    mMoves(mStartMoveIndex + (i << 1) + 1)

  /** Sets the move score at specified index.
   * @param i 			the index.
   * @param score		the score.
   */
  def setScore(i: Int, score: Int): Unit =
    mMoves(mStartMoveIndex + (i << 1) + 1) = score 

  /** Swap two moves and their score for the specified indexes.
   * @param i			the index for first move.
   * @param j			the index for second move.
   */
  def swap(i: Int, j: Int): Unit = {
    val ii = mStartMoveIndex + (i << 1)
    val jj = mStartMoveIndex + (j << 1)
    val tmpMove = mMoves(ii)
    val tmpScore = mMoves(ii + 1)
    mMoves(ii) = mMoves(jj)
    mMoves(ii + 1) = mMoves(jj + 1)
    mMoves(jj) = tmpMove
    mMoves(jj + 1) = tmpScore
  }
}