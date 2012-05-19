package pl.luckboy.liftchess.engine

/** A piece class.
 * 
 * @author Łukasz Szpakowski
 */
final class Piece private(val id: Int, val name: String) extends EnumValue
{
  def toPieceOption: PieceOption =
    PieceOption(id)
}

/** A piece singleton.
 * 
 * @author Łukasz Szpakowski
 */
object Piece
{
  val Pawn = new Piece(0, "P")
  val Knight = new Piece(1, "N")
  val Bishop = new Piece(2, "B")
  val Rook = new Piece(3, "R")
  val Queen = new Piece(4, "Q")
  val King = new Piece(5, "K")
  
  private val Values = makeArray(Pawn, Knight, Bishop, Rook, Queen, King)
  
  def apply(id: Int): Piece = 
    Values(id)
    
  def makeArray[T](p: T, n: T, b: T, r: T, q: T, k: T)(implicit m: ClassManifest[T]): Array[T] = 
    Array(p, n, b, r, q, k)
    
  implicit def toPieceOption(piece: Piece): PieceOption =
    PieceOption(piece.id)
    
  def values: Set[Piece] =
    Values.toSet
}
