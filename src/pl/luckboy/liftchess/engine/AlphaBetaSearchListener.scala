package pl.luckboy.liftchess.engine

/** A listener trait for alpha beta algorithm.
 * 
 * @author Łukasz Szpakowski
 */
trait AlphaBetaSearchListener
{
  /** Searcher invokes this method if it enters to node.
   * @param i		the level of node (zero is root).
   * @param depth	the depth.
   * @param alpha	the alpha.
   * @param beta	the beta.
   */
  def onPreorder(i: Int, depth: Int, alpha: Int, beta: Int): Unit
  
  /** Searcher invokes this method if it leaves from node.
   * @param i		the level of node (zero is root).
   * @param depth	the depth.
   * @param alpha	the alpha.
   * @param beta	the beta.
   */
  def onPostorder(i: Int, depth: Int, alpha: Int, beta: Int): Unit
  
  /** Searcher invokes this method if it changes alpha.
   * @param i		the level of node (zero is root).
   * @param depth	the depth.
   * @param alpha	the alpha.
   * @param move	the move that been reason to change alpha.
   */
  def onAlphaChange(i: Int, depth: Int, alpha: Int, move: Move): Unit
  
  /** Searcher invokes this method if it cut.
   * @param i		the level of node (zero is root).
   * @param depth	the depth.
   * @param alpha	the alpha.
   * @param move	the move that been reason to cut.
   */
  def onBetaCut(i: Int, depth: Int, alpha: Int, move: Move): Unit
}