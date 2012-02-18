package pl.luckboy.liftchess.engine

/** Cecha przeszukiwania drzewa gry.
 * 
 * @author Łukasz Szpakowski
 */
trait Searcher[TSearchInput]
{  
  /** Evaluator searchera. */
  def evaluator: Evaluator

  /** Stan gry searchera. */
  def gameState: GameState
  
  /** Ustawia stan gry searchera. */
  def gameState_=(gs: GameState): Unit
  
  /** Listener przeszukiwania. */
  def searchListener: SearchListener
  
  /** Ustawia listener przeszukiwania. */
  def searchListener_=(listener: SearchListener): Unit
  
  /** Przeszukuje drzewa gry na daną głebokość.
   * @param depth		głębokość.
   * @param input		dane wejściowe dla przeszukiwania.
   * @return			wynik przeszukiwania.
   */
  def search(depth: Int, input: TSearchInput): Int
}