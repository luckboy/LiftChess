package pl.luckboy.liftchess
import scala.util.control.Exception._
import pl.luckboy.liftchess.engine._

/** A singleton for the board builder of FEN.
 * 
 * @author Łukasz Szpakowski
 */
object FENBuilder 
{
  def apply(s: String): BoardBuilder = {
    val fields = s.split(" +").toSeq
    require(fields.size >= 4 && fields.size <= 6)
    val Seq(ps, ss, cs, enps) = fields.take(4)
    val extraFields = (4 to 5).map(fields.lift).zip(Seq("0", "1")).map { case (so, ds) => so.getOrElse(ds) }
    // pieces
    val rows = ps.split("/").toSeq
    require(rows.size == 8)
    val pieces = rows.foldLeft(Seq[SidePieceOption]()) {
      (pieces, row) => 
         row.foldLeft(pieces) {
           (pieces, c) =>
             val pieceRow = SidePiece.values.find { _.name == c.toString }.map { Seq[SidePieceOption](_) }.getOrElse {
               require(c >= '1' && c <= '8')
               (1 to c.toString.toInt).map { _ => SidePieceOption.None }
             }
             require(pieceRow.size == 8)
             pieces ++ pieceRow
         } 
    }
    // side
    require(Side.values.exists { _.name == ss })
    val side = Side.values.find { _.name == ss }.get
    // castling
    require(cs == "-" || cs.forall { c => "KQkq".forall(c ==) })
    val castlingPair = cs.foldLeft(Castling.NoneCastling, Castling.NoneCastling) {
      (pair, c) => c match {
         case 'K' => (pair._1 | Castling.KingsideCastling, pair._2)
         case 'Q' => (pair._1 | Castling.QueensideCastling, pair._2)
         case 'k' => (pair._1, pair._2 | Castling.KingsideCastling)
         case 'q' => (pair._1, pair._2 | Castling.QueensideCastling)
         case '-' => pair
      }
    }
    // en passant
    require(SquareOption.values.exists { _.name == ss })
    val enPassant = SquareOption.values.find { _.name == enps }.get
    // halfmove clock and fullmove number
    require(extraFields.forall { t => catching(classOf[NumberFormatException]).opt { t.toInt }.isDefined })
    val Seq(halfmoveClock, fullmoveNumber) = extraFields.map { _.toInt }
    // board builder
    BoardBuilder(pieces, side, castlingPair, enPassant, halfmoveClock, fullmoveNumber)
  }
  
  def unapply(builder: BoardBuilder): Option[String] =
    Some(builder.toFENString)
  
  /** Converts to the FEN string.
   * @param builder		the board builder.
   * @return			a string.
   */
  def toFENString(builder: BoardBuilder): String = {
    val ps = (0 to 7).map {
      row =>
        (0 to 7).foldLeft(("", 0)) {
          case ((s, n), col) =>
            builder.pieces(Square(row, col)) match {
              case SidePieceOption.None => (s, n + 1)
              case piece                => ((if(n == 0) "" else n.toString) + piece, 0)
          }  
        }._1
    }.mkString("/")
    val ss = builder.side.toString
    val cs = (
        builder.castling(Side.White).toString.toUpperCase +
        builder.castling(Side.Black).toString.toLowerCase).replace("-", "")
    val enps = builder.enPassant.toString
    val hmvcs = builder.halfmoveClock.toString
    val fmvns = builder.fullmoveNumber.toString
    ps + " " + ss + " " + cs + " " + enps + " " + hmvcs + " " + fmvns
  }
}