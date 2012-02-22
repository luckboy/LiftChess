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

  /** Składa pola bicia pionka na danym polu.
   * @param sq 			pole.
   * @param side		strona.
   * @param z			wartość początkowa.
   * @param f			funkcja składania.
   * @return			wynik składania.
   */
  def foldPawnCaptureSquare[T](sq: Int, side: Side)(z: T)(f: (T, Int) => T): T = throw new Exception
  
  /** Składa pola ruchu bierki na danym polu (z wyjątkiem bic pionków).
   * @param sq 			pole.
   * @param side		strony.
   * @param piece		bierka.
   * @param z			wartość początkowa.
   * @param p			funkcja przerwania (gdy false przerywa składanie dla aktualnej linii ale nie samo składanie).
   * @param f			funkcja składania przed przerwaniem linii.
   * @param g			funkcja składania po przerwaniu linii.
   * @return			wynik składania.
   */
  def foldMoveSquares[T](sq: Int, side: Side, piece: Piece)(z: T)(p: (T, Int) => Boolean)(f: (T, Int) => T)(g: (T, Int) => T): T = throw new Exception
}