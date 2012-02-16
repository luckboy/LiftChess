package pl.luckboy.liftchess.engine

/** Klasa strony.
 * 
 * @author Łukasz Szpakowski
 */
class Side private(val id: Int, val name: String) extends EnumValue

/** Singleton strony.
 * 
 * @author Łukasz Szpakowski
 */
object Side
{
  val White = new Side(0, "w")
  val Black = new Side(1, "b")
  
  private val Values = Array(White, Black)
  
  def apply(id: Int): Side =
    Values(id)
}
