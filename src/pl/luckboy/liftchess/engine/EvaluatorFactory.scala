package pl.luckboy.liftchess.engine

/** Fabryka funkcji oceniającej.
 * 
 * @author Łukasz Szpakowski
 */
trait EvaluatorFactory[T]
{
  type Params
  
  def apply(params: Params): T
}