package pl.luckboy.liftchess.engine

/** Cecha nasłuchiwania przeszukiwania drzewa gry dla okna.
 * 
 * @author Łukasz Szpakowski
 */
trait AlphaBetaSearchListener
{
  /** Wykonawana gdy wchodzi do węzła.
   * @param i		poziom na którym jest wezeł (zero to korzeń).
   * @param depth	głębokość.
   * @param alpha	alfa.
   * @param beta	beta.
   */
  def onPreorder(i: Int, depth: Int, alpha: Int, beta: Int): Unit
  
  /** Wywoływana gdy wychodzi do węzła.
   * @param i		poziom na którym jest wezeł (zero to korzeń).
   * @param depth	głębokość.
   * @param alpha	alfa.
   * @param beta	beta.
   */
  def onPostorder(i: Int, depth: Int, alpha: Int, beta: Int): Unit
  
  /** Wywoływana gdy alfa się zmieniła.
   * @param i		poziom na którym jest wezeł (zero to korzeń).
   * @param depth	głębokość.
   * @param alpha	alfa.
   * @param move	ruch, przez który została zmieniona alfa.
   */
  def onAlphaChange(i: Int, depth: Int, alpha: Int, move: Move): Unit
  
  /** Wywoływana gdy alfa się zmieniła.
   * @param i		poziom na którym jest wezeł (zero to korzeń).
   * @param depth	głębokość.
   * @param alpha	alfa.
   * @param move	ruch, przez który została zmieniona alfa.
   */
  def onBetaCut(i: Int, depth: Int, alpha: Int, move: Move): Unit
}