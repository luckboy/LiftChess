package pl.luckboy.liftchess.engine

/** A treat for evalution function.
 * 
 * @author Łukasz Szpakowski
 */
trait Evaluator 
{
  /** Podaje ocenę dla planszy.
   * @param bd			plansza.
   * @return			ocena.
   */
  def evaluate(bd: Board): Int
}