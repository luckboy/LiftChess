package pl.luckboy.liftchess.engine

/** Klasa silnika.
 * 
 * @author Łukasz Szpakowski
 */
trait Engine
{  
  /** Stan gry silnika. */
  def gameState: GameState
  
  /** Ustawia stan gry silnika. */
  def gameState_=(gs: GameState): Unit
  
  /** Nalepszy ruch według silnika. */
  def bestMove: Move
}