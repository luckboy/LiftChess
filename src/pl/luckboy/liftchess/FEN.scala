package pl.luckboy.liftchess
import pl.luckboy.liftchess.engine.Board

/** A singleton for the FEN.
 * 
 * @author Łukasz Szpakowski
 */
object FEN 
{
  def apply(s: String): Board = 
    FENBuilder(s)
  
  def unapply(bd: Board): Option[String] = 
    FENBuilder.unapply(BoardBuilder.fromBoard(bd))
  
  /** Converts the board to a FEN string.
   * @param bd			the board.
   * @return			a FEN string.
   */
  def toFENString(bd: Board): String =
    FENBuilder.toFENString(BoardBuilder.fromBoard(bd))
}