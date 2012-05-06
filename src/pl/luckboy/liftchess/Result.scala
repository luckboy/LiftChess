package pl.luckboy.liftchess
import pl.luckboy.liftchess.engine._

/** A singleton for the game result.
 * 
 * @author Łukasz Szpakowski
 */
object Result extends Enumeration
{
  val Unknown = Value("*")
  val WhiteWin = Value("1-0")
  val BlackWin = Value("0-1")
  val Draw = Value("1/2-1/2")
  
  def win(side: Side) =
    if(side == Side.White) WhiteWin else BlackWin
    
  def lose(side: Side) =
    if(side == Side.White) BlackWin else WhiteWin
}