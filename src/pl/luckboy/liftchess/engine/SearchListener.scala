package pl.luckboy.liftchess.engine

/**
 * @author Łukasz Szpakowski
 */
trait SearchListener
{
  def onPreorder(i: Int, depth: Int): Unit
  
  def onPostorder(i: Int, depth: Int): Unit
  
  def onBetterMoveSet(i: Int, depth: Int, move: Move): Unit
}