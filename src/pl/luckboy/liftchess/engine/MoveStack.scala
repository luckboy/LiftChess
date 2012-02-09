package pl.luckboy.liftchess.engine

/**
 * @author Łukasz Szpakowski
 */
class MoveStack
{
  /**
   * Generuje pseudo legalne ruchy i wkłada na stos.
   * @param bd			plansza.
   */
  def generatePseudoLegalMoves(bd: Board): Unit = throw new Exception

  /**
   * Generuje pseudo legalne ruchy które mogą być potencjalnie dobre i wkłada na stos.
   * @param bd			plansza.
   */
  def generatePseudoLegalGoodMoves(bd: Board): Unit = throw new Exception

  /**
   * Zdejmuje ruchy z stosu.
   */
  def popMoves(): Unit = throw new Exception
  
  /**
   * Wkłada pseudo legalne ruchy i wykonuje funkcje. Następnie po wykonaniu funkcji zdejmuje ruchy z stosu.
   * @param bd			plansza.
   * @param z			wartość początkowa.
   * @param f			funkcja.
   * @return			wynik funkcji.
   */
  def succPseudoLegalMoves[T](bd: Board)(z: T)(f: (MoveStack) => T): T = throw new Exception
  
  /**
   * Wkłada pseudo legalne ruchy, które są potencjalie dobre i wykonuje funkcje. Następnie po wykonaniu funkcji 
   * zdejmuje ruchy z stosu.
   * @param bd			plansza.
   * @param z			wartość początkowa.
   * @param f			funkcja.
   * @return			wynik funkcji.
   */
  def succPseudoLegalGoodMoves[T](bd: Board)(z: T)(f: (MoveStack) => T): T = throw new Exception

  /**
   * Liczba ruchów.
   */
  def size: Int = throw new Exception
  
  /**
   * Podaje ruch o danym indeksie.
   * @param i			indeks.
   * @return			ruch.
   */
  def apply(i: Int): Move = throw new Exception
}