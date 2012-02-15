package pl.luckboy.liftchess.engine

/**
 * @author Łukasz Szpakowski
 */
class SideOption private(val id: Int, val name: String) extends EnumValue
{
  def foldLeft[T](z: T)(f: (T, Side) => T): T =
    if(id != 0) f(z, Side(id)) else z
}

/**
 * @author Łukasz Szpakowski
 */
object SideOption
{
  val None = new SideOption(-1, ".")
  val White = new SideOption(Side.White.id, Side.White.name)
  val Black = new SideOption(Side.Black.id, Side.Black.name)
  
  private val Values = Array(None, White, Black)
  
  def apply(id: Int): SideOption =
    Values(id + 1)
}