package pl.luckboy.liftchess.engine

/**
 * @author Łukasz Szpakowski
 */
class MoveStack 
{
  /**
   * Składa posortowane ruchy pseudo legalne dla strony która ma ruch.
   * @param bd			plansza.
   * @param mvEval		funkcja oceniająca ruch.
   * @param z			wartość począntkowa.
   * @param p			funkcja przerwania (false gdy przerywa).
   * @param f			funkcja składania.
   * @return 			wynik składania.
   */
  def foldSortedPseudoLegalMoves[T](bd: Board)(mvEval: (Move) => Int)(z: T)(p: (T, Move) => T)(f: (T, Move) => T): T = throw new Exception

  /**
   * Składa posortowane potencjalnie dobre ruchy pseudo legalne dla strony która ma ruch.
   * @param bd			plansza.
   * @param mvEval		funkcja oceniająca ruch.
   * @param z			wartość począntkowa.
   * @param p			funkcja przerwania (false gdy przerywa).
   * @param f			funkcja składania.
   * @return 			wynik składania.
   */
  def foldSortedGoodPseudoLegalMoves[T](bd: Board)(mvEval: (Move) => Int)(z: T)(p: (T, Move) => T)(f: (T, Move) => T): T = throw new Exception
}