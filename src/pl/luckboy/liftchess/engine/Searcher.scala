package pl.luckboy.liftchess.engine

/**
 * @author Łukasz Szpakowski
 */
trait Searcher 
{
  def search(depth: Int, alpha: Int, beta: Int): Int
}