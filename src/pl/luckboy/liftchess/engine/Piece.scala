package pl.luckboy.liftchess.engine

/** Klasa bierki.
 * 
 * @author Łukasz Szpakowski
 */
class Piece private(val id: Int, val name: String) extends EnumValue

/** Singleton bierki.
 * 
 * @author Łukasz Szpakowski
 */
object Piece
{
  val Pawn = new Piece(1, "P")
  val Knight = new Piece(2, "N")
  val Bishop = new Piece(3, "B")
  val Rook = new Piece(4, "R")
  val Queen = new Piece(5, "Q")
  val King = new Piece(6, "K")
  
  private val Values = Array(Pawn, Knight, Bishop, Rook, Queen, King)
  
  def apply(id: Int): Piece = 
    Values(id - 1)
}
