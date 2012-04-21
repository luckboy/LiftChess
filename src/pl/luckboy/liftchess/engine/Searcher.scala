package pl.luckboy.liftchess.engine

/** A trait for the searches game tree. 
 * 
 * @author Łukasz Szpakowski
 */
trait Searcher
{  
  /** The listener type. */
  type SearchListener
  
  /** The evaluator for the searcher. */
  def evaluator: Evaluator

  /** The game state for the searcher. */
  def gameState: GameState

  /** Sets the game state for the searcher. */
  def gameState_=(gs: GameState): Unit

  /** The search listener. */
  def searchListener: SearchListener

  /** Sets the search listener. */
  def searchListener_=(listener: SearchListener): Unit

  /** Searches game tree for the specified depth.
   * @param depth		the depth.
   * @return			the search result.
   */
  def search(depth: Int): Int
}