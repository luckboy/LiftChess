package pl.luckboy.liftchess.engine

/** A class for state game.
 * 
 * @author Łukasz Szpakowski
 */
class GameState
{
  /** The board. */
  def board: Board = throw new Exception
  
  /** This method is like unsafeFoldSuccessor in Board but only for game state.
   * @param move		the move
   * @param z			the start move.
   * @param f			the folding function.
   * @return			the result of function or start value.
   */
  def unsafeFoldSuccessor[T](move: Move)(z: T)(f: (T, GameState) => T): T = throw new Exception

  /** This method is like unsafeFoldNullSuccessor in Board but only for game state.
   * @param z			the start move.
   * @param f			the folding function.
   * @return			the result of function or start value.
   */
  def unsafeFoldNullSuccessor[T](z: T)(f: (T, GameState) => T): T = throw new Exception

  /** This method is like unsafeFoldSuccessorWithoutHashKey in Board but only for game state.
   * @param move		the move
   * @param z			the start move.
   * @param f			the folding function.
   * @return			the result of function or start value.
   */
  def unsafeFoldSuccessorWithoutHashKey[T](move: Move)(z: T)(f: (T, GameState) => T): T = throw new Exception

  /** This method is like unsafeFoldNullSuccessorWithoutHashKey in Board but only for game state.
   * @param z			the start move.
   * @param f			the folding function.
   * @return			the result of function or start value.
   */
  def unsafeFoldNullSuccessorWithoutHashKey[T](move: Move)(z: T)(f: (T, GameState) => T): T = throw new Exception

  /** Folds sorted successors for game state.
   * @param mvStack		the moves (move buffer).
   * @param mvEval		the evaluation function for move.
   * @param z			the start value.
   * @param p			the function of stopping that tests before makes move (if this function returns false, there 
   *                    stops folding).
   * @param q			the function of stopping that tests after makes move (if this function returns false, there
   *                    stops folding).
   * @param f			the folding function.
   * @return			the folding result.
   */
  def foldSortedSuccessors[T](mv: MoveStack)(mvEval: (Move) => Int)(z: T)(p: (T, GameState, Move) => Boolean)(q: (T, GameState, Move) => Boolean)(f: (T, GameState, Move) => T): T = throw new Exception

  /** Folds sorted successors that may be potentially good for game state.
   * @param mvStack		the moves (move buffer).
   * @param mvEval		the evaluation function for move.
   * @param z			the start value.
   * @param p			the function of stopping that tests before makes move (if this function returns false, there 
   *                    stops folding).
   * @param q			the function of stopping that tests after makes move (if this function returns false, there
   *                    stops folding).
   * @param f			the folding function.
   * @return			the folding result.
   */
  def foldSortedGoodSuccessors[T](mvStack: MoveStack)(mvEval: (Move) => Int)(z: T)(p: (T, GameState, Move) => Boolean)(q: (T, GameState, Move) => Boolean)(f: (T, GameState, Move) => Boolean): T = throw new Exception
  
  /** Returns true if side won.
   * @param				the side
   * @return			true if side won.
   */
  def isWin(side: Side): Boolean = throw new Exception

  /** Returns true if side lost.
   * @param side		the side.
   * @return			true if side lost.
   */
  def isLose(side: Side): Boolean = throw new Exception
  
  /** Returns true if there is draw.
   * @return			true if there is draw.
   */
  def isDraw: Boolean = throw new Exception  
}