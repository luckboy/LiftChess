package pl.luckboy.liftchess

/** A singleton for the board builder of FEN.
 * 
 * @author Łukasz Szpakowski
 */
object FENBuilder 
{
  def apply(s: String): BoardBuilder = throw new Exception
  
  def unapply(builder: BoardBuilder): Option[String] = 
    FEN.unapply(builder.toBoard)
}