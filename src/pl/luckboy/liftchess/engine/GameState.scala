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
   * Podobnie jak unsafeSucc w Board tylko dla gry.
   * @param	move		ruch.
   * @param z			wartość początkowa.
   * @param f			funkcja.
   * @return			wynik funkcji lub wartość początkowa.
   */
  def unsafeSucc[T](move: Move)(z: T)(f: (GameState) => T): T = throw new Exception

  /**
   * Podobnie jak unsafeSuccNull w Board tylko dla gry.
   * @param z			wartość początkowa.
   * @param f			funkcja.
   * @return			wynik funkcji lub wartość początkowa.
   */
  def unsafeSuccNull[T](z: T)(f: (GameState) => T): T = throw new Exception

  /**
   * Składa posortowane następniki stanu gry.
   * @param mvStack		ruchy (bufor ruchów).
   * @param mvEval		funkcja oceniająca ruchy.
   * @param z			wartość początkowa.
   * @param p			funkcja przerwania wykonywana przed ruchem (gdy false to przerwanie).
   * @param q			funkcja przerwania wykonywana po ruchem (gdy false to przerwanie).
   * @param f			funkcja składania.
   * @return 			wynik składania.
   */
  def foldSortedSuccessors[T](mv: MoveStack)(mvEval: (Move) => Int)(z: T)(p: (T, GameState, Move) => Boolean)(q: (T, GameState, Move) => Boolean)(f: (T, GameState, Move) => T): T = throw new Exception

  /**
   * Składa posortowane potencjalnie dobre następniki stanu gry.
   * @param mvStack		ruchy (bufor ruchów).
   * @param mvEval		funkcja oceniająca ruchy.
   * @param z			wartość początkowa.
   * @param p			funkcja przerwania wykonywana przed ruchem (gdy false to przerwanie).
   * @param q			funkcja przerwania wykonywana po ruchem (gdy false to przerwanie).
   * @param f			funkcja składania.
   * @return 			wynik składania.
   */
  def foldSortedGoodSuccessors[T](mvStack: MoveStack)(mvEval: (Move) => Int)(z: T)(p: (T, GameState, Move) => Boolean)(q: (T, GameState, Move) => Boolean)(f: (T, GameState, Move) => Boolean): T = throw new Exception
  
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