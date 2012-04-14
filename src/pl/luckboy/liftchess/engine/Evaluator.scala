package pl.luckboy.liftchess.engine

/** A treat for evalution function.
 * 
 * @author Łukasz Szpakowski
 */
trait Evaluator 
{
  /** Returns evaluation of board.
   * @param bd			the board.
   * @return			the evaluation.
   */
  def evaluate(bd: Board): Int
}