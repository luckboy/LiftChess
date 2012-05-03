package pl.luckboy.liftchess
import pl.luckboy.liftchess.engine.Board

/** A singleton for the FEN.
 * 
 * @author Łukasz Szpakowski
 */
object FEN 
{
  def apply(s: String): Board = FENBuilder(s)
  
  def unapply(bd: Board): Option[String] = throw new Exception
}