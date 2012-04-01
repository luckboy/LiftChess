package pl.luckboy.liftchess.engine

/** A class for optional piece.
 * 
 * @author Łukasz Szpakowski
 */
final class PieceOption private(val id: Int, val name: String) extends EnumValue
{
  @inline
  def foldLeft[@specialized T](z: T)(f: (T, Piece) => T): T =
    if(id != 6) f(z, Piece(id)) else z
}

/** A singleton for optional piece.
 * 
 * @authpr Łukasz Szpakowski
 */
object PieceOption
{
  val None = new PieceOption(6, "_")
  val Pawn = new PieceOption(Piece.Pawn.id, Piece.Pawn.name)
  val Knight = new PieceOption(Piece.Knight.id, Piece.Knight.name)
  val Bishop = new PieceOption(Piece.Bishop.id, Piece.Bishop.name)
  val Rook = new PieceOption(Piece.Rook.id, Piece.Rook.name)
  val Queen = new PieceOption(Piece.Queen.id, Piece.Queen.name)
  val King = new PieceOption(Piece.King.id, Piece.King.name)
  
  private val Values = makeArray(Pawn, Knight, Bishop, Rook, Queen, King, None)
  
  def apply(id: Int): PieceOption = 
    Values(id)
    
  def makeArray[T](p: T, n: T, b: T, r: T, q: T, k: T, none: T)(implicit m: ClassManifest[T]): Array[T] =
    Piece.makeArray(p, n, b, r, q, k) :+ none
}
