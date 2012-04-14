package pl.luckboy.liftchess.engine

/** A trait for alpha beta algorithm. 
 * 
 * @author Łukasz Szpakowski
 */
trait AlphaBetaSearcher extends Searcher
{
  override type SearchListener = AlphaBetaSearchListener
  
  /** Searches game tree for specified depth with specified window.
   * @param depth		the depth.
   * @param alpha		the alpha.
   * @param beta		the beta.
   * @return			the search result.
   */
  def searchWithAlphaBeta(depth: Int, alpha: Int, beta: Int): Int
}