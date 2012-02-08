package pl.luckboy.liftchess.engine

/**
 * Klasa stanu gry.
 * 
 * @author Łukasz Szpakowski
 */
class GameState
{
  /**
   * Plasza w grze.
   */
  def board: Board = throw new Exception
  
  /**
   * Podobnie jak unsageSucc w Board tylko dla gry.
   * @param	move		ruch.
   * @param z			wartość początkowa.
   * @param f			funkcja.
   * @return			wynik funkcji lub wartość początkowa.
   */
  def unsafeSucc[T](move: Move)(z: T)(f: (GameState) => T): T = throw new Exception

  /**
   * Podobnie jak unsageSuccNullMove w Board tylko dla gry.
   * @param z			wartość początkowa.
   * @param f			funkcja.
   * @return			wynik funkcji lub wartość początkowa.
   */
  def unsafeSuccNullMove[T](z: T)(f: (GameState) => T): T = throw new Exception
  
  /**
   * Podaje true jeśli strona wygrała grę.
   * @param side		strona.
   * @return			jeśli wygrała grę to true.
   */
  def isWin(side: Side.Value): Boolean = throw new Exception

  /**
   * Podaje true jeśli strona przegrała grę.
   * @param side		strona.
   * @return			jeśli przegrała grę to true.
   */
  def isLose(side: Side.Value): Boolean = throw new Exception
  
  /**
   * Podaje true jeśli jest remis.
   * @return 			jeśli remis to true.
   */
  def isDraw: Boolean = throw new Exception
}