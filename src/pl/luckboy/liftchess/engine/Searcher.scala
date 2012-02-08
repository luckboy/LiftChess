package pl.luckboy.liftchess.engine

/**
 * Cecha przeszukiwania drzewa gry.
 * 
 * @author Łukasz Szpakowski
 */
trait Searcher 
{
  /**
   * Evaluator searchera.
   */
  def evaluator: Evaluator

  /**
   * Stan gry searchera.
   */
  def gameState: GameState
  
  /**
   * Ustawia stan gry searchera.
   */
  def gameState_=(gs: GameState): Unit
  
  /**
   * Przeszukuje drzewa gry na daną głebokość.
   * @param depth		głębokość.
   * @param alpha		alfa.
   * @param beta		beta.
   * @return			wynik przeszukiwania.
   */
  def search(depth: Int, alpha: Int, beta: Int): Int
}