package pl.luckboy.liftchess.engine

/** Cecha przeszukiwania drzewa gry dla okna.
 * 
 * @author Łukasz Szpakowski
 */
trait AlphaBetaSearcher extends Searcher
{
  /** Przeszukiwanie drzewa gry na daną głębokość z określonym oknem.
   * @param depth		głębokość.
   * @param alpha		alfa.
   * @param beta		beta.
   * @return			wynik przeszukiwania.
   */
  def searchWithAlphaBeta(depth: Int, alpha: Int, beta: Int): Int
}