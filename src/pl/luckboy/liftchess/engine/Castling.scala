package pl.luckboy.liftchess.engine

/** A class for castling. 
 * 
 * @author Łukasz Szpakowski
 */
final class Castling private(val id: Int, val name: String) extends EnumValue
{
  @inline
  def unary_~ : Castling =
    Castling(id ^ 3)
  
  @inline
  def &(castling: Castling): Castling =
    Castling(id & castling.id)
  
  @inline
  def |(castling: Castling): Castling =
    Castling(id | castling.id)
}

/** A class for castling.
 * 
 * @author Łukasz Szpakowski
 */
object Castling
{
  val NoneCastling = new Castling(0, "-")
  val KingsideCastling = new Castling(1, "K")
  val QueensideCastling = new Castling(2, "Q")
  val AllCastling = new Castling(3, "KQ")
  
  private val Values = Array(NoneCastling, KingsideCastling, QueensideCastling, AllCastling)
  
  def apply(id: Int): Castling =
    Values(id)
}