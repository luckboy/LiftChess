package pl.luckboy.liftchess.engine

/** A engine class.
 * 
 * @author Łukasz Szpakowski
 */
trait Engine
{  
  /** The game state for engine. */
  def gameState: GameState
  
  /** Sets game state for engine */
  def gameState_=(gs: GameState): Unit

  /** The best move from engine. */
  def bestMove: Move
}