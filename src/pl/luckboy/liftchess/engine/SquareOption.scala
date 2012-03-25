package pl.luckboy.liftchess.engine

/** Klasa pola opcjonalnego.
 * 
 * @author Łukasz Szpakowski
 */
final class SquareOption private(val id: Int, val name: String) extends EnumValue
{
  def square =
    id
  
  @inline
  def foldLeft[@specialized T](z: T)(f: (T, Int) => T): T = 
    if(id == -1) z else f(z, id)    
}

/** Singleton pola opcjonalnego.
 * 
 * @author Łukasz Szpakowski
 */
object SquareOption
{
  val None = new SquareOption(-1, "-")
  
  private val Values = (0 to 63).map { 
    sq => new SquareOption(sq, ('a' to 'h')(Square.toColumn(sq)).toString + (8 - Square.toRow(sq)))
  }
  
  def apply(sq: Int) =
    Values(sq)
}