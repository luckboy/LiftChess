package pl.luckboy.liftchess.engine

/** A factory for evaluation function.
 * 
 * @author Łukasz Szpakowski
 */
trait EvaluatorFactory[T]
{
  type Params
  
  def apply(params: Params): T
}