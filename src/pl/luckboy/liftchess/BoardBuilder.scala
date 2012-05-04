package pl.luckboy.liftchess
import pl.luckboy.liftchess.engine._

/** A class for board builder.
 * 
 * @author Łukasz Szpakowski
 */
case class BoardBuilder(
    pieces: Seq[SidePieceOption],
    side: Side,
    castlingPair: (Castling, Castling),
    enPassant: SquareOption,
    halfmoveClock: Int,
    fullmoveNumber: Int)
{
  /** Creates a copy of board builder with new pieces.
   * @param newPieces	the new pieces.
   * @return			the new builder.
   */
  def updatedPieces(newPieces: Seq[SidePieceOption]): BoardBuilder =
    BoardBuilder(newPieces, side, castlingPair, enPassant, halfmoveClock, fullmoveNumber)
  
  /** Creates a copy of board builder with new piece at specified square.
   * @param sq			the square.
   * @param newPiece	the new piece.
   * @return			the new builder.
   */
  def updatedPieceAt(sq: Int, newPiece: SidePieceOption): BoardBuilder = 
    BoardBuilder(pieces.updated(sq, newPiece), side, castlingPair, enPassant, halfmoveClock, fullmoveNumber)    
    
  /** Creates a copy of board builder with new side.
   * @param newSide		the new side.
   * @return			the new builder.
   */
  def updatedSide(newSide: Side): BoardBuilder =
  	BoardBuilder(pieces, newSide, castlingPair, enPassant, halfmoveClock, fullmoveNumber)
 
  /** Creates a copy of board builder with new pair of castling.
   * @param newCastlingPair	the new pair of castling.
   * @return				the new builder.
   */
  def updatedCastlingPair(newCastlingPair: (Castling, Castling)): BoardBuilder =
    BoardBuilder(pieces, side, newCastlingPair, enPassant, halfmoveClock, fullmoveNumber)

  /** The castling for the specified side.
   * @param side		the sided.
   * @return			the castling.
   */
  def castling(side: Side): Castling =
    if(side == Side.White) castlingPair._1 else castlingPair._2
  
  /** Creates a copy of board builder with new castling for the specified side.
   * @param side		the side.
   * @param castling	the castling.
   * @return			the new builder.
   */
  def updatedCastling(side: Side, castling: Castling): BoardBuilder =
    updatedCastlingPair(if(side == Side.White) (castling, castlingPair._2) else (castlingPair._1, castling))

  /** Creates a copy of board builder with new square of en passant.
   * @param newCastlingPair	the new square of en passant.
   * @return				the new builder.
   */
  def updatedEnPassant(newEnPassant: SquareOption): BoardBuilder =
    BoardBuilder(pieces, side, castlingPair, newEnPassant, halfmoveClock, fullmoveNumber)
    
  /** Creates a copy of board builder with new halfmove clock.
   * @param newHalfmoveClock	the new halfmove clock.
   * @return					the new builder.
   */
  def updatedHalfmoveClock(newHalfmoveClock: Int): BoardBuilder =
  	BoardBuilder(pieces, side, castlingPair, enPassant, newHalfmoveClock, fullmoveNumber)

  /** Creates a copy of board builder with new fullmove number.
   * @param newHalfmoveClock	the new fullmove number.
   * @return					the new builder.
   */
  def updatedFullmoveNumber(newFullmoveNumber: Int): BoardBuilder =
  	BoardBuilder(pieces, side, castlingPair, enPassant, halfmoveClock, newFullmoveNumber)
  	
  /** Converts to a board. */
  def toBoard: Board =
    Board(pieces, side, castlingPair, enPassant, halfmoveClock, fullmoveNumber)
}

/** A singleton for the board builder.
 * 
 * @author Łukasz Szpakowski
 */
object BoardBuilder
{
  implicit def toBoard(builder: BoardBuilder): Board = 
    builder.toBoard
    
  implicit def toImplicitBoard(builder: BoardBuilder): ImplicitBoard = 
    builder.toBoard
    
  /** Returns the board builder of initial position. */
  def initialBoardBuilder: BoardBuilder = throw new Exception
  
  /** Creates a board builder from the board. */
  def fromBoard(bd: Board): BoardBuilder =
    BoardBuilder((0 to 63).map { bd(_) }, bd.side, (bd.castling(Side.White), bd.castling(Side.Black)), bd.enPassant, bd.halfmoveClock, bd.fullmoveNumber)
}