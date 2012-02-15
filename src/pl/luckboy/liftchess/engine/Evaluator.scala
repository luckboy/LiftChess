package pl.luckboy.liftchess.engine

/** Cecha funkcji oceniającej.
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