package pl.luckboy.liftchess.engine

/** Cecha przeszukiwania drzewa gry.
 * 
 * @author Łukasz Szpakowski
 */
trait Searcher
{  
  /** Typ listenera */
  type SearchListener
  
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
   * @return			wynik przeszukiwania.
   */
  def search(depth: Int): Int
}