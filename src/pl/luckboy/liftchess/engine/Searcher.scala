package pl.luckboy.liftchess.engine

/** A trait for searches game tree. 
 * 
 * @author Łukasz Szpakowski
 */
trait Searcher
{  
  /** The listener type. */
  type SearchListener
  
  /** The evaluator for searcher. */
  def evaluator: Evaluator

  /** The game state for searcher. */
  def gameState: GameState

  /** Sets the game state for searcher. */
  def gameState_=(gs: GameState): Unit

  /** The search listener. */
  def searchListener: SearchListener

  /** Sets the search listener. */
  def searchListener_=(listener: SearchListener): Unit

  /** Searches game tree for specified depth.
   * @param depth		the depth.
   * @return			the search result.
   */
  def search(depth: Int): Int
}