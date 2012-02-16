package pl.luckboy.liftchess.engine

/** Singleton pole.
 * 
 * @author Łukasz Szpakowski
 */
object Square
{
  /** Utworzy pole z kolumny i wiersza.
   * @param row			wiersz.
   * @param col			kolmna.
   * @return			pole.
   */
  def apply(row: Int, col: Int): Int =
    (row << 3) + col

  /** Podaje kolumne z pola.
   * @param	sq 			pole.
   * @return			kolumna.
   */
  def toColumn(sq: Int): Int =
    sq & 7

  /** Podaje wiersz z pola.
   * @param sq		pole.
   */
  def toRow(sq: Int): Int =
    sq >> 3
}