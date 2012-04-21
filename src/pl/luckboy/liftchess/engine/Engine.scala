package pl.luckboy.liftchess.engine

/** A engine class.
 * 
 * @author Łukasz Szpakowski
 */
trait Engine
{  
  /** The game state for the engine. */
  def gameState: GameState
  
  /** Sets the game state for the engine */
  def gameState_=(gs: GameState): Unit

  /** The best move from the engine. */
  def bestMove: Move
}