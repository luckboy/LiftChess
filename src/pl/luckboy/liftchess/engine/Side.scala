package pl.luckboy.liftchess.engine

/** A side class.
 * 
 * @author Łukasz Szpakowski
 */
final class Side private(val id: Int, val name: String) extends EnumValue
{
  /** Returns the opposite side. */
  @inline
  def opposite: Side =
    Side(id ^ 1)
    
  /** Converts to a side option. */
  def toSideOption: SideOption =
    SideOption(id)
}

/** A side singleton.
 * 
 * @author Łukasz Szpakowski
 */
object Side
{
  val White = new Side(0, "w")
  val Black = new Side(1, "b")
  
  private val Values = makeArray(White, Black)
  
  def apply(id: Int): Side =
    Values(id)
    
  def makeArray[T](w: T, b: T)(implicit m: ClassManifest[T]) =
    Array(w, b)
    
  implicit def toSideOption(side: Side): SideOption =
    SideOption(side.id)
    
  def values: Set[Side] =
    Values.toSet
}
