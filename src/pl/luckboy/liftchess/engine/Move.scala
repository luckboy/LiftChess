package pl.luckboy.liftchess.engine

/** A move class.
 * 
 * @author Łukasz Szpakowski
 */
final case class Move(piece: Piece, source: Int, destination: Int, promotionPiece: PieceOption, moveType: MoveType)

/** A class of move type.
 * 
 * @author Łukasz Szpakowski
 */
final class MoveType private(val id: Int, val name: String) extends EnumValue

/** A singleton for move type.
 * 
 * @author Łukasz Szpakowski
 */
object MoveType
{
  val NormalMove = new MoveType(0, "NormalMove") 
  val Capture = new MoveType(1, "Capture")
  val EnPassant = new MoveType(2, "EnPassant")
  val KingsideCastling = new MoveType(3, "KingsideCastling")
  val QueensideCastling = new MoveType(4, "QueensideCastling")
  
  val Values = Array(NormalMove, Capture, EnPassant, KingsideCastling, QueensideCastling)
  
  def apply(id: Int): MoveType =
    Values(id)
}

/** A singleton for normal move.
 * 
 * @author Łukasz Szpakowski
 */
object NormalMove
{
  def apply(piece: Piece, src: Int, dst: Int, promPiece: PieceOption): Move =
    Move(piece, src, dst, promPiece, MoveType.NormalMove)
    
  def unapply(move: Move): Option[(Piece, Int, Int, PieceOption)] =
    move match {
      case Move(piece, src, dst, promPiece, MoveType.NormalMove) => Some(piece, src, dst, promPiece) 
      case _                                                     => None
    }
}

/** A capture singleton.
 * 
 * @author Łukasz Szpakowski
 */
object Capture
{
  def apply(piece: Piece, src: Int, dst: Int, promPiece: PieceOption): Move =
    Move(piece, src, dst, promPiece, MoveType.Capture)
    
  def unapply(move: Move): Option[(Piece, Int, Int, PieceOption)] =
    move match {
      case Move(piece, src, dst, promPiece, MoveType.Capture) => Some(piece, src, dst, promPiece) 
      case _                                                  => None
    }
}

/** A singleton for en passant.
 * 
 * @author Łukasz Szpakowski
 */
object EnPassant
{
  def apply(src: Int, dst: Int): Move =
    Move(Piece.Pawn, src, dst, PieceOption.None, MoveType.EnPassant)
    
  def unapply(move: Move): Option[(Int, Int)] =
    move match {
      case Move(Piece.Pawn, src, dst, PieceOption.None, MoveType.EnPassant) => Some(src, dst) 
      case _                                                                => None
    }
}

/** A singleton for kingside castling.
 * 
 * @author Łukasz Szpakowski
 */
object KingsideCastling
{
  def apply(): Move =
    Move(Piece.King, 4, 6, PieceOption.None, MoveType.KingsideCastling)
    
  def unapply(move: Move): Boolean =
    move.moveType == MoveType.KingsideCastling
}

/** A singleton for queenside castling.
 * 
 * @author Łukasz Szpakowski
 */
object QueensideCastling
{
  def apply(): Move =
    Move(Piece.King, 4, 2, PieceOption.None, MoveType.QueensideCastling)
    
  def unapply(move: Move): Boolean =
    move.moveType == MoveType.QueensideCastling
}