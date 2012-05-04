package pl.luckboy.liftchess

/** A singleton for the game result.
 * 
 * @author Łukasz Szpakowski
 */
object Result extends Enumeration
{
  val Abort = Value("*")
  val WhiteWin = Value("1-0")
  val BlackWin = Value("0-1")
  val Draw = Value("1/2-1/2")
}