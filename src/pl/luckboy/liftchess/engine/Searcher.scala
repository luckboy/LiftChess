package pl.luckboy.liftchess.engine

/**
 * @author Łukasz Szpakowski
 */
trait Searcher 
{
  def evaluator: Evaluator
  
  def search(depth: Int, alpha: Int, beta: Int): Int
}