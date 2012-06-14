/*******************************************************************************
 * Copyright (C) 2012 Łukasz Szpakowski.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

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
         val pieceRow = row.foldLeft(Seq[SidePieceOption]()) {
           (pieceRow, c) =>
             val pieceSeq = SidePiece.values.find { _.name == c.toString }.map { Seq[SidePieceOption](_) }.getOrElse {
               require(c >= '1' && c <= '8')
               (1 to c.toString.toInt).map { _ => SidePieceOption.None }
             }
             pieceRow ++ pieceSeq
         } 
         require(pieceRow.size == 8)
         pieces ++ pieceRow
    }
    // side
    require(Side.values.exists { _.name == ss })
    val side = Side.values.find { _.name == ss }.get
    // castling
    require(cs == "-" || cs.forall { c => "KQkq".exists(c ==) })
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
    require(SquareOption.values.exists { _.name == enps })
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
        val (s, n) = (0 to 7).foldLeft(("", 0)) {
          case ((s, n), col) =>
            builder.pieces(Square(row, col)) match {
              case SidePieceOption.None => (s, n + 1)
              case piece                => (s + (if(n == 0) "" else n.toString) + piece, 0)
          }  
        }
        s + (if(n == 0) "" else n.toString)
    }.mkString("/")
    val ss = builder.side.toString
    val tmpCs = (
        builder.castling(Side.White).toString.toUpperCase +
        builder.castling(Side.Black).toString.toLowerCase).replace("-", "")
    val cs = if(tmpCs.isEmpty) "-" else tmpCs
    val enps = builder.enPassant.toString
    val hmvcs = builder.halfmoveClock.toString
    val fmvns = builder.fullmoveNumber.toString
    ps + " " + ss + " " + cs + " " + enps + " " + hmvcs + " " + fmvns
  }
}
