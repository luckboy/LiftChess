package pl.luckboy.liftchess.engine

/** A class for the optional side.
 * 
 * @author Łukasz Szpakowski
 */
final class SideOption private(val id: Int, val name: String) extends EnumValue
{
  /** Returns the opposite side */
  def opposite: SideOption =
    if(id != 2) SideOption(id ^ 1) else this

  /** Folds side.
   * @param z			the start value.
   * @param	f			the folding function.
   * @return			the folding result.
   */
  @inline
  def foldLeft[@specialized T](z: T)(f: (T, Side) => T): T =
    if(id != 2) f(z, Side(id)) else z
}

/** A singleton for the optional side.
 * 
 * @author Łukasz Szpakowski
 */
object SideOption
{
  val None = new SideOption(2, "_")
  val White = new SideOption(Side.White.id, Side.White.name)
  val Black = new SideOption(Side.Black.id, Side.Black.name)
  
  private val Values = Array(White, Black, None)
  
  def apply(id: Int): SideOption =
    Values(id)
    
  def makeArray[T](w: T, b: T, none: T)(implicit m: ClassManifest[T]): Array[T] = 
    Array(w, b, none)
    
  def values: Set[SideOption] =
    Values.toSet
}