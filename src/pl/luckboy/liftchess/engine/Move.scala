package pl.luckboy.liftchess.engine

/** Klasa ruchu.
 * 
 * @author Łukasz Szpakowski
 */
case class Move(piece: Piece, source: Int, destination: Int, promotionPiece: PieceOption, flags: Int)

/** Singleton ruchu.
 * 
 * @author Łukasz Szpakowski
 */
object Move
{
  val NoneFlag = 0
  val CaptureFlag = 1
  val KingsideCastlingFlag = 2
  val QueensideCastlingFlag = 4
}

/** Singleton ruchu normalnego.
 * 
 * @author Łukasz Szpakowski
 */
object NormalMove
{
  def apply(piece: Piece, src: Int, dst: Int, promPiece: PieceOption): Move =
    Move(piece, src, dst, promPiece, Move.NoneFlag)
    
  def unapply(move: Move): Option[(Piece, Int, Int, PieceOption)] =
    move match {
      case Move(piece, src, dst, promPiece, Move.NoneFlag) => Some(piece, src, dst, promPiece) 
      case _                                               => None
    }
}

/** Singleton bicia.
 * 
 * @author Łukasz Szpakowski
 */
object Capture
{
  def apply(piece: Piece, src: Int, dst: Int, promPiece: PieceOption): Move =
    Move(piece, src, dst, promPiece, Move.CaptureFlag)
    
  def unapply(move: Move): Option[(Piece, Int, Int, PieceOption)] =
    move match {
      case Move(piece, src, dst, promPiece, Move.CaptureFlag) => Some(piece, src, dst, promPiece)
      case _                                                  => None
    }
}

/** Singleton roszady krótkiej.
 * 
 * @author Łukasz Szpakowski
 */
object KingsideCastling
{
  def apply(): Move =
    Move(Piece.King, 4, 6, PieceOption.None, Move.KingsideCastlingFlag)
    
  def unapply(move: Move): Boolean =
    (move.flags & Move.KingsideCastlingFlag) != 0
}

/** Singleton roszady długiej.
 * 
 * @author Łukasz Szpakowski
 */
object QueensideCastling
{
  def apply(): Move =
    Move(Piece.King, 4, 2, PieceOption.None, Move.QueensideCastlingFlag)
    
  def unapply(move: Move): Boolean =
    (move.flags & Move.QueensideCastlingFlag) != 0
}