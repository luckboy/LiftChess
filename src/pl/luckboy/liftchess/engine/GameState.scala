
package pl.luckboy.liftchess.engine

/** A class for state game.
 * 
 * @author Łukasz Szpakowski
 */
final class GameState(bd: Board)
{
  import GameState._

  /** The board. */
  protected val mBoard = bd.clone()
  
  /** The index for mRepHashKeys array. */
  protected var mRepIndex = 0
  
  /** The array contains hash key for repetitions. */
  protected val mRepHashKeys = new Array[Long](MaxRepHashKeys)
  
  /** The array contains the counters for repetitions. If counter of array has value 0, hash key of counter hasn't correct 
   * value. */
  protected val mRepCounters = Array.fill(MaxRepHashKeys)(0)
  
  /** Whether can check repetition of position. */
  protected var mCanCheckRep = false

  /** The number of legal moves. */
  protected var mNumberOfLegalMoves = -1
  
  /** Pushes hash key. 
   * @param key			the hash key.
   * @return			the saved hash key.
   */
  protected def pushHashKey(key: Long) = {
    val savedKey = mRepHashKeys(mRepIndex)
    mRepHashKeys(mRepIndex) = key
    mRepCounters(mRepIndex) += 1
    //mRepIndex = (mRepIndex + 1) % MaxRepHashKeys
    mRepIndex += 1
    if(mRepIndex >= MaxRepHashKeys) mRepIndex = 0
    savedKey
  }
  
  /** Pops hash key and pushes saved hash key into him place. 
   * @param savedHashKey	the saved hash key.
   */
  protected def popHashKey(savedHashKey: Long) = {
    //mRepIndex = (mRepIndex + MaxRepHashKeys - 1) % MaxRepHashKeys
    mRepIndex -= 1
    if(mRepIndex < 0) mRepIndex = MaxRepHashKeys - 1
    mRepCounters(mRepIndex) -= 1
    mRepHashKeys(mRepIndex) = savedHashKey    
  }

  /** The board for game state. */
  def board: Board =
    mBoard.clone()
  
  /** This method is like unsafeFoldSuccessor in Board but only for game state.
   * @param move		the move
   * @param z			the start move.
   * @param f			the folding function.
   * @return			the result of function or start value.
   */
  @inline
  def unsafeFoldSuccessor[@specialized T](move: Move)(z: T)(f: (T, GameState) => T): T = {
    val savedHashKey = pushHashKey(mBoard.hashKey)
    val savedNumberOfLegalMoves = mNumberOfLegalMoves
    mNumberOfLegalMoves = -1
    try {
      mBoard.unsafeFoldSuccessor(move)(z) { (x, _) => f(x, this) }
    } finally {
      mNumberOfLegalMoves = savedNumberOfLegalMoves
      popHashKey(savedHashKey)
    }
  }

  /** This method is like unsafeFoldNullSuccessor in Board but only for game state.
   * @param z			the start move.
   * @param f			the folding function.
   * @return			the result of function or start value.
   */
  @inline
  def unsafeFoldNullSuccessor[@specialized T](z: T)(f: (T, GameState) => T): T = {
    val savedHashKey = pushHashKey(mBoard.hashKey)
    val savedNumberOfLegalMoves = mNumberOfLegalMoves
    mNumberOfLegalMoves = -1
    try {
      mBoard.unsafeFoldNullSuccessor(z) { (x, _) => f(x, this) }
    } finally {
      mNumberOfLegalMoves = savedNumberOfLegalMoves
      popHashKey(savedHashKey)      
    }
  }
  
  /** This method is like unsafeFoldSuccessorWithoutHashKey in Board but only for game state.
   * @param move		the move
   * @param z			the start move.
   * @param f			the folding function.
   * @return			the result of function or start value.
   */
  @inline
  def unsafeFoldSuccessorWithoutHashKey[@specialized T](move: Move)(z: T)(f: (T, GameState) => T): T = {
    val savedNumberOfLegalMoves = mNumberOfLegalMoves
    val savedCanCheckRep = mCanCheckRep
    mNumberOfLegalMoves = -1
    mCanCheckRep = false
    try {
      mBoard.unsafeFoldSuccessorWithoutHashKey(move)(z) { (x, _) => f(x, this) }
    } finally {
      mCanCheckRep = savedCanCheckRep
      mNumberOfLegalMoves = savedNumberOfLegalMoves
    }
  }

  /** This method is like unsafeFoldNullSuccessorWithoutHashKey in Board but only for game state.
   * @param z			the start value.
   * @param f			the folding function.
   * @return			the result of function or start value.
   */
  @inline
  def unsafeFoldNullSuccessorWithoutHashKey[@specialized T](z: T)(f: (T, GameState) => T): T = {
    val savedNumberOfLegalMoves = mNumberOfLegalMoves
    val savedCanCheckRep = mCanCheckRep
    mNumberOfLegalMoves = -1
    mCanCheckRep = false
    try {
      mBoard.unsafeFoldNullSuccessorWithoutHashKey(z) { (x, _) => f(x, this) }
    } finally {
      mCanCheckRep = savedCanCheckRep
      mNumberOfLegalMoves = savedNumberOfLegalMoves
    }
  }

  /** Just, sorts one move.
   * @param mvStack		the moves (move buffer).
   * @param i			the index of move.
   */
  protected def sortMove(mvStack: MoveStack, i: Int) = {
    var j = i + 1
    while(j < mvStack.size) {
      if(mvStack.score(i) < mvStack.score(j)) mvStack.swap(i, j)
      j += 1
    }
  }
  
  /** Folds sorted successors for game state.
   * @param mvStack		the moves (move buffer).
   * @param mvEval		the evaluation function for move.
   * @param z			the start value.
   * @param p			the function of stopping that tests before evaluates f function (if this function returns false, there 
   *                    stops folding).
   * @param q			the function of stopping that tests after evaluates f function (if this function returns false, there
   *                    stops folding).
   * @param f			the folding function.
   * @return			the folding result.
   */
  @inline
  def foldSortedSuccessors[@specialized T](mvStack: MoveStack)(mvEval: (Move) => Int)(z: T)(p: (T, GameState, Move) => Boolean)(q: (T, GameState, Move) => Boolean)(f: (T, GameState, Move) => T): T = {
    mvStack.generatePseudoLegalMovesWithPopMoves(mBoard) {
      var i = 0
      val n = mvStack.size
      while(i < n) {
        mvStack.setScore(i, mvEval(mvStack.move(i)))
        i += 1
      }
      var y = z
      var dontStop = true
      i = 0
      var nLegalMoves = 0
      while(i < n && dontStop) {
        sortMove(mvStack, i)
        val move = mvStack.move(i)
        y = unsafeFoldSuccessor(move)(z) { 
          (x, gs) => 
            var y = x
            dontStop = p(y, gs, move)
            if(!dontStop) {
              y = f(x, gs, move) 
              dontStop = q(y, gs, move)
            }
            nLegalMoves += 1
            y
        }
        i += 1
      }
      mNumberOfLegalMoves = nLegalMoves
      y
    }
  }

  /** Folds sorted successors that may be potentially good for game state.
   * @param mvStack		the moves (move buffer).
   * @param mvEval		the evaluation function for move.
   * @param z			the start value.
   * @param p			the function of stopping that tests before evaluates f function (if this function returns false, there 
   *                    stops folding).
   * @param q			the function of stopping that tests after evaluates f function (if this function returns false, there
   *                    stops folding).
   * @param f			the folding function.
   * @return			the folding result.
   */
  @inline
  def foldSortedGoodSuccessors[@specialized T](mvStack: MoveStack)(mvEval: (Move) => Int)(z: T)(p: (T, GameState, Move) => Boolean)(q: (T, GameState, Move) => Boolean)(f: (T, GameState, Move) => T): T = {
    mvStack.generatePseudoLegalGoodMovesWithPopMoves(mBoard) {
      var i = 0
      val n = mvStack.size
      while(i < n) {
        mvStack.setScore(i, mvEval(mvStack.move(i)))
        i += 1
      }
      var y = z
      var dontStop = true
      i = 0
      while(i < n && dontStop) {
        sortMove(mvStack, i)
        val move = mvStack.move(i)
        y = unsafeFoldSuccessor(move)(z) { 
          (x, gs) => 
            var y = x
            dontStop = p(y, gs, move)
            if(!dontStop) {
              y = f(x, gs, move) 
              dontStop = q(y, gs, move)
            }
            y
        }
        i += 1
      }
      y
    }
  }
  
  /** Folds sorted successors for game state but this method doesn't calculate hash key.
   * @param mvStack		the moves (move buffer).
   * @param mvEval		the evaluation function for move.
   * @param z			the start value.
   * @param p			the function of stopping that tests before evaluates f function (if this function returns false, there 
   *                    stops folding).
   * @param q			the function of stopping that tests after evaluates f function (if this function returns false, there
   *                    stops folding).
   * @param f			the folding function.
   * @return			the folding result.
   */
  @inline
  def foldSortedSuccessorsWithoutHashKey[@specialized T](mvStack: MoveStack)(mvEval: (Move) => Int)(z: T)(p: (T, GameState, Move) => Boolean)(q: (T, GameState, Move) => Boolean)(f: (T, GameState, Move) => T): T = {
    mvStack.generatePseudoLegalMovesWithPopMoves(mBoard) {
      var i = 0
      val n = mvStack.size
      while(i < n) {
        mvStack.setScore(i, mvEval(mvStack.move(i)))
        i += 1
      }
      var y = z
      var dontStop = true
      i = 0
      var nLegalMoves = 0
      while(i < n && dontStop) {
        sortMove(mvStack, i)
        val move = mvStack.move(i)
        y = unsafeFoldSuccessorWithoutHashKey(move)(z) { 
          (x, gs) => 
            var y = x
            dontStop = p(y, gs, move)
            if(!dontStop) {
              y = f(x, gs, move) 
              dontStop = q(y, gs, move)
            }
            nLegalMoves += 1
            y
        }
        i += 1
      }
      mNumberOfLegalMoves = nLegalMoves
      y
    }    
  }
  
  /** Folds sorted successors that may be potentially good for game state but this method doesn't calculate hash key.
   * @param mvStack		the moves (move buffer).
   * @param mvEval		the evaluation function for move.
   * @param z			the start value.
   * @param p			the function of stopping that tests before evaluates f function (if this function returns false, there 
   *                    stops folding).
   * @param q			the function of stopping that tests after evaluates f function (if this function returns false, there
   *                    stops folding).
   * @param f			the folding function.
   * @return			the folding result.
   */
  @inline
  def foldSortedGoodSuccessorsWithoutHashKey[@specialized T](mvStack: MoveStack)(mvEval: (Move) => Int)(z: T)(p: (T, GameState, Move) => Boolean)(q: (T, GameState, Move) => Boolean)(f: (T, GameState, Move) => T): T = {
    mvStack.generatePseudoLegalGoodMovesWithPopMoves(mBoard) {
      var i = 0
      val n = mvStack.size
      while(i < n) {
        mvStack.setScore(i, mvEval(mvStack.move(i)))
        i += 1
      }
      var y = z
      var dontStop = true
      i = 0
      while(i < n && dontStop) {
        sortMove(mvStack, i)
        val move = mvStack.move(i)
        y = unsafeFoldSuccessorWithoutHashKey(move)(z) { 
          (x, gs) => 
            var y = x
            dontStop = p(y, gs, move)
            if(!dontStop) {
              y = f(x, gs, move) 
              dontStop = q(y, gs, move)
            }
            y
        }
        i += 1
      }
      y
    }    
  }

  /** Returns number of legal moves. */
  protected def countLegalMoves = {
    val mvStack = new MoveStack(1, 256)
    mvStack.generatePseudoLegalMovesWithPopMoves(mBoard) {
      (0 until mvStack.size).foldLeft(0) { (sum, i) => sum + mBoard.unsafeFoldSuccessor(mvStack.move(i))(0) { (_, _) => 1 } }
    }
  }
  
  /** Returns true if side won.
   * @param				the side
   * @return			true if side won.
   */
  def isWin(side: Side): Boolean =
    isLose(side.opposite)

  /** Returns true if side lost.
   * @param side		the side.
   * @return			true if side lost.
   */
  def isLose(side: Side): Boolean = {
    if(side eq mBoard.side) {
      if(mNumberOfLegalMoves == -1) mNumberOfLegalMoves = countLegalMoves
      mNumberOfLegalMoves == 0 && mBoard.inCheck
    } else {
      false
    }
  }
  
  /** Returns true if there is draw.
   * @return			true if there is draw.
   */
  def isDraw: Boolean = {
    if(mNumberOfLegalMoves == -1) mNumberOfLegalMoves = countLegalMoves
    if(mNumberOfLegalMoves == 0 && mBoard.inCheck) {
      false
    } else {
      if((mNumberOfLegalMoves == 0 && !mBoard.inCheck) || mBoard.halfmoveClock >= 100) {
        true
      } else {
        if(mCanCheckRep) {
          // Checks whether draw by repetition of position.
          var i = 0
          var j = (mRepIndex + MaxRepHashKeys - 1) % MaxRepHashKeys
          while(i < MaxRepHashKeys && mRepCounters(j) > 0) {
            if(mRepHashKeys(j) == mBoard.hashKey) return true
            i += 1
            //j = (j + MaxRepHashKeys - 1) % MaxRepHashKeys
            j -= 1
            if(j < 0) j = MaxRepHashKeys - 1
          }
        }
        false
      }
    }
  }
}

object GameState
{
  /** The maximal number of the hash key for the repetition of position. */
  val MaxRepHashKeys: Int = 32
}