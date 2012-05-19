package pl.luckboy.liftchess.engine

/** A class for the optional square.
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

/** A singleton for the optional square.
 * 
 * @author Łukasz Szpakowski
 */
object SquareOption
{
  val None = new SquareOption(-1, "-")
  
  private val Values = (0 to 63).map { sq => new SquareOption(sq, Square.toString(sq)) }.toArray
  
  def apply(sq: Int) =
    Values(sq)
    
  def values: Set[SquareOption] =
    Values.toSet + None
}