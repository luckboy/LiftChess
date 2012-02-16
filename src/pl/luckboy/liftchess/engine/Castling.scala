package pl.luckboy.liftchess.engine

/** Klasa roszad. 
 * 
 * @author Łukasz Szpakowski
 */
class Castling private(val id: Int, val name: String) extends EnumValue
{
  def unary_~ : Castling =
    Castling(id ^ 3)
  
  def &(castling: Castling): Castling =
    Castling(id & castling.id)
    
  def |(castling: Castling): Castling =
    Castling(id | castling.id)
}

/** Singleton roszad.
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