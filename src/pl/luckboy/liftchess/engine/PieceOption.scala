package pl.luckboy.liftchess.engine

/**
 * @author Łukasz Szpakowski
 */
class PieceOption private(val id: Int, val name: String) extends EnumValue
{
  def foldLeft[T](z: T)(f: (T, Piece) => T): T =
    if(id != 0) f(z, Piece(id)) else z
}

/**
 * @authpr Łukasz Szpakowski
 */
object PieceOption
{
  val None = new PieceOption(0, ".")
  val Pawn = new PieceOption(Piece.Pawn.id, Piece.Pawn.name)
  val Knight = new PieceOption(Piece.Knight.id, Piece.Knight.name)
  val Bishop = new PieceOption(Piece.Bishop.id, Piece.Bishop.name)
  val Rook = new PieceOption(Piece.Rook.id, Piece.Rook.name)
  val Queen = new PieceOption(Piece.Queen.id, Piece.Queen.name)
  val King = new PieceOption(Piece.King.id, Piece.King.name)
  
  private val Values = Array(None, Pawn, Knight, Bishop, Rook, Queen, King)
  
  def apply(id: Int): PieceOption = 
    Values(id)
}
