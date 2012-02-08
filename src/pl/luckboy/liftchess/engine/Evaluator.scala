package pl.luckboy.liftchess.engine

/**
 * Cecha funkcji oceniającej.
 */
trait Evaluator 
{
  /**
   * Podaje ocenę dla planszy.
   * @param bd			plansza.
   * @return			ocena.
   */
  def evaluate(bd: Board): Int
}