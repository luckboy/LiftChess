package pl.luckboy.liftchess.engine

/** A square singleton.
 * 
 * @author Łukasz Szpakowski
 */
object Square
{
  /** Creates a square from column and row.
   * @param row			the row.
   * @param col			the column.
   * @return 			the square.
   */
  @inline
  def apply(row: Int, col: Int): Int =
    (row << 3) + col

  /** Returns a column of square.
   * @param sq			the square.
   * @return			the column.
   */
  @inline
  def toColumn(sq: Int): Int =
    sq & 7

  /** Returns a row of square.
   * @param sq			the square.
   * @return			the row.
   */
  @inline
  def toRow(sq: Int): Int =
    sq >> 3

  val Mailbox88 = (0 to 7).map { row => (0 to 7).map { Square(row, _) }.toArray ++ Array.fill(8)(-1) }.flatten.toArray
  
  val Lookup88 = (0 to 63).map { sq => (Square.toRow(sq) << 4) + Square.toColumn(sq) }.toArray
  
  val NonSlidingSteps = Piece.makeArray[Array[Int]](
      Array(),
      Array(-33, -31, -18, -14, 14, 18, 31, 33),
      Array(),
      Array(),
      Array(),
      Array(-17, -16, -15, -1, 1, 15, 16, 17)
      )
      
  val SlidingSteps = Piece.makeArray[Array[Int]](
      Array(),
      Array(),
      Array(-17, -15, 15, 17),
      Array(-16, -1, 1, 16),
      Array(-17, -16, -15, -1, 1, 15, 16, 17),
      Array()
      )
      
  val IsSliding = Piece.makeArray(false, false, true, true, true, false)

  val PawnSteps = Side.makeArray(
      Array(-17, -15, -16, -32),
      Array(15, 17, 16, 32)
      )

  /** Folds the squares of pawn captures for the specified square. 
   * @param sq			the square.
   * @param side		the side.
   * @param z			the start value.
   * @param p			the stopping function (if that function returns false, folding stops).
   * @param f			the folding function.
   * @return			the folding result.
   */
  @inline 
  def foldPawnCaptureSquares[@specialized T](sq: Int, side: Side)(z: T)(p: (T, Int) => Boolean)(f: (T, Int) => T): T = {
    val dst1 = Lookup88(sq) + (if(side eq Side.White) -17 else 15)
    val dst2 = Lookup88(sq) + (if(side eq Side.White) -15 else 17)
    val y1 = if((dst1 & 0x88) == 0) {
      val dst64 = Mailbox88(dst1)
      if(!p(z, dst64)) return z
      f(z, dst64)
    } else {
      z
    }
    val y2 = if((dst2 & 0x88) == 0) {
      val dst64 = Mailbox88(dst2)
      if(!p(y1, dst64)) return y1
      f(y1, dst64)
    } else {
      y1
    }
    y2
  }

  /** Folds the squares of pawn moves for the specified squares.
   * @param sq			the square.
   * @param side		the side.
   * @param z			the start value.
   * @param p			the stopping function (if that function returns false, folding stops).
   * @param f			the folding function.
   * @return			the folding result.
   */
  @inline
  def foldPawnMoveSquares[@specialized T](sq: Int, side: Side)(z: T)(p: (T, Int) => Boolean)(f: (T, Int) => T): T = {
    val step = if(side eq Side.White) -16 else 16
    val rowDst2 = if(side eq Side.White) 0x40 else 0x30
    val dst1 = Lookup88(sq) + step
    if((dst1 & 0x88) == 0 && p(z, Mailbox88(dst1))) {
      val y = f(z, Mailbox88(dst1))
      val dst2 = dst1 + step
      if((dst2 & 0xf0) == rowDst2 && p(y, Mailbox88(dst2))) 
        f(y, Mailbox88(dst2))
      else
        y
    } else {
      z
    }
  }

  /** Folds the square of non-sliding moves for the specified square.
   * @param sq			the square.
   * @param piece		the piece.
   * @param z			the start value.
   * @param p			the stopping function (if that function returns false, folding stops).
   * @param f			the folding function.
   * @return			the folding result.
   */
  @inline
  def foldNonSlidingMoveSquares[@specialized T](sq: Int, piece: Piece)(z: T)(p: (T, Int) => Boolean)(f: (T, Int) => T): T = {
    var y = z
    var i = 0
    while(i < 8) {
      val dst = Lookup88(sq) + NonSlidingSteps(piece.id)(i)
      if((dst & 0x88) == 0) {
        val dst64 = Mailbox88(dst)
        if(!p(y, dst64)) return y
        y = f(y, dst64)
      }
      i += 1
    }
    y
  }
  
  /** Folds the squares of sliding moves for the specified square.
   * @param sq			the square.
   * @param piece		the piece.
   * @param z			the start value.
   * @param p			the stopping function (if this function returns false, there stops folding for all lines).
   * @param l			the line function.
   * @param q			the function of line stopping (if this function returns false, there just stops folding for current
   *                    line, but there doesn't stop folding for all lines).
   * @param f			the folding function that folds before there stops folding for one line.
   * @param g			the folding function that folds after there stops folding for one line.
   * @return			the folding result.
   */
  @inline
  def foldSlidingMoveSquares[@specialized T](sq: Int, piece: Piece)(z: T)(p: (T) => Boolean)(l: (T) => T)(q: (T, Int) => Boolean)(f: (T, Int) => T)(g: (T, Int) => T): T = {
    var y = z
    var i = 0
    while(i < SlidingSteps(piece.id).length && p(y)) {
      val step = SlidingSteps(piece.id)(i)
      var dst = Lookup88(sq) + step
      y = l(y)
      while((dst & 0x88) == 0 && q(y, Mailbox88(dst))) {
        y = f(y, Mailbox88(dst))
        dst += step
      }
      if((dst & 0x88) == 0) y = g(y, Mailbox88(dst))
      i += 1
    }
    y
  }
  
  /** Folds the move squares for the specified square. In case piece is pawn, just evaluates f function for non-captures and 
   * break condition is satisfied. Just evaluates g function for captures and break condition isn't satisfied.
   * @param sq			the square.
   * @param side		the side.
   * @param	piece		the piece.
   * @param z			the start value.
   * @param p			the function of line stopping (if this function returns false, there stops folding for current line
   *                    but there doesn't stop folding for all line).
   * @param f			the folding function that folds before there stops folding for line.
   * @param g			the folding function that folds after there stops folding for line.
   * @return			the folding result.
   */
  @inline
  def foldMoveSquares[@specialized T](sq: Int, side: Side, piece: Piece)(z: T)(p: (T, Int) => Boolean)(f: (T, Int) => T)(g: (T, Int) => T): T =
    if(IsSliding(piece.id)) {
      var y = z
      var i = 0
      while(i < SlidingSteps(piece.id).length) {
        val step = SlidingSteps(piece.id)(i)
        var dst = Lookup88(sq) + step
        while((dst & 0x88) == 0  && p(y, Mailbox88(dst))) {
          y = f(y, Mailbox88(dst))
          dst += step
        }
        if((dst & 0x88) == 0) y = g(y, Mailbox88(dst))
        i += 1
      }
      y
    } else {
      if(piece eq Piece.Pawn) {
        val step = if(side eq Side.White) -16 else 16
        var y = z
        var i = 0
        val n = if(Square.toRow(sq) == (if(side eq Side.White) 6 else 1)) 4 else 3
        while(i < n) {
          val dst = Lookup88(sq) + PawnSteps(side.id)(i)
          if((dst & 0x88) == 0) {
            val dst64 = Mailbox88(dst)
            val b = p(y, dst64)
            if(i >= 2) {
              if(!b) return y
              y = f(y, dst64)
            }
            if(i < 2 && !b) y = g(y, dst64) 
          }
          i += 1
        }
        y
      } else {
        var y = z
        var i = 0
        while(i < 8) {
          val dst = Lookup88(sq) + NonSlidingSteps(piece.id)(i)
          if((dst & 0x88) == 0) {
            val dst64 = Mailbox88(dst)
            y = if(p(y, dst64)) f(y, dst64) else g(y, dst64)
          }
          i += 1
        }
        y
      }
    }
  
  /** Converts the square to a string.
   * @param sq			the square.
   * @return			the string.
   */
  def toString(sq: Int): String =
    ('a' to 'h')(Square.toColumn(sq)).toString + (8 - Square.toRow(sq))
}