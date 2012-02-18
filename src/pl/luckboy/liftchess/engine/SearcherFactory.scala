package pl.luckboy.liftchess.engine

/**
 * @author Łukasz Szpakowski
 */
trait SearcherFactory[T]
{
  type Params
  
  def apply(eval: Evaluator, params: Params): T
}