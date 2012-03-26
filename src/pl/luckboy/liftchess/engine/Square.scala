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
  @inline
  def apply(row: Int, col: Int): Int =
    (row << 3) + col

  /** Podaje kolumne z pola.
   * @param	sq 			pole.
   * @return			kolumna.
   */
  @inline
  def toColumn(sq: Int): Int =
    sq & 7

  /** Podaje wiersz z pola.
   * @param sq		pole.
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
  
  /** Składa pola bic pionka na danym polu.
   * @param sq 			pole.
   * @param side		strona.
   * @param z			wartość początkowa.
   * @param p			funkcja przerwania (gdy false przerywa).
   * @param f			funkcja składania.
   * @return			wynik składania.
   */
  @inline 
  def foldPawnCaptureSquares[@specialized T](sq: Int, side: Side)(z: T)(p: (T, Int) => Boolean)(f: (T, Int) => T): T = {
    val dst1 = Lookup88(sq) + (if(side == Side.White) -17 else 15)
    val dst2 = Lookup88(sq) + (if(side == Side.White) -15 else 17)
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

  /** Składa pola ruchów pionka na danym polu.
   * @param sq 			pole.
   * @param strona		strona.
   * @param z			wartość początkowa.
   * @param p			funkcja przerwania linii (gdy false przerywa składanie dla aktualnej linii ale nie samo składanie).
   * @param f			funkcja składania przed przerwaniem linii.
   * @return			wynik składania.
   */
  @inline
  def foldPawnMoveSquares[@specialized T](sq: Int, side: Side)(z: T)(p: (T, Int) => Boolean)(f: (T, Int) => T): T = {
    val step = if(side == Side.White) -16 else 16
    val rowDst2 = if(side == Side.White) 0x40 else 0x30
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

  /** Składa pola nieciągłe ruchy na danym polu.
   * @param sq 			pole.
   * @param piece		bierka.
   * @param z			wartość początkowa.
   * @param p			funkcja przerwania (gdy false przerywa).
   * @param f			funkcja składania.
   * @return			wynik składania.
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
  
  /** Składa pola ciągłego ruchu bierki na danym polu.
   * @param sq 			pole.
   * @param piece		bierka.
   * @param z			wartość początkowa.
   * @param p			funkcja przerwania składania(gdy false przerywa).
   * @param l			funkcja składania linii.
   * @param q			funkcja przerwania linii (gdy false przerywa składanie dla aktualnej linii ale nie samo składanie).
   * @param f			funkcja składania przed przerwaniem linii.
   * @param g			funkcja składania po przerwaniu linii.
   * @return			wynik składania.
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
  
  /** Składa pola ruchu bierki na danym polu. W przypadku pionka funkcja f jest obliczana tylko dla nie bicia oraz gdy 
   * warunek przerwania jest spełniony. Zaś funkcja g jest oblicza tylko dla bic oraz gdy warunek przerwania nie jest 
   * spełniony. 
   * @param sq 			pole.
   * @param side		strony.
   * @param piece		bierka.
   * @param z			wartość początkowa.
   * @param p			funkcja przerwania linii (gdy false przerywa składanie dla aktualnej linii ale nie samo składanie).
   * @param f			funkcja składania przed przerwaniem linii.
   * @param g			funkcja składania po przerwaniu linii.
   * @return			wynik składania.
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
      if(piece == Piece.Pawn) {
        val step = if(side == Side.White) -16 else 16
        var y = z
        var i = 0
        val n = if(Square.toRow(sq) == (if(side == Side.White) 6 else 1)) 4 else 3
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
}