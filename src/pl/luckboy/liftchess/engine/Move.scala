package pl.luckboy.liftchess.engine

/** Klasa ruchu.
 * 
 * @author Łukasz Szpakowski
 */
case class Move(piece: Piece, source: Int, destination: Int, promotionPiece: PieceOption, moveType: MoveType)

/** Klasa typu ruchu.
 * 
 * @author Łukasz Szpakowski
 */
class MoveType private(val id: Int, val name: String) extends EnumValue

/** Singleton typu ruchu.
 * 
 * @author Łukasz Szpakowski
 */
object MoveType
{
  val NormalMove = new MoveType(0, "NormalMove") 
  val EnPassant = new MoveType(1, "EnPassant")
  val KingsideCastling = new MoveType(2, "KingsideCastling")
  val QueensideCastling = new MoveType(3, "QueensideCastling")
  
  val Values = Array(NormalMove, EnPassant, KingsideCastling, QueensideCastling)
  
  def apply(id: Int): MoveType =
    Values(id)
}

/** Singleton ruchu normalnego.
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

/** Singleton bicia w przelocie.
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

/** Singleton roszady krótkiej.
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

/** Singleton roszady długiej.
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