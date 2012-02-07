package pl.luckboy.liftchess.engine

/**
 * @author Łukasz Szpakowski
 */
trait SearcherFactory
{
  def apply(eval: Evaluator): Searcher
}