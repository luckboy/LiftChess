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

package pl.luckboy.liftchess.engine

/** A move class.
 * 
 * @author Łukasz Szpakowski
 */
final case class Move(piece: Piece, source: Int, destination: Int, promotionPiece: PieceOption, moveType: MoveType)
{
  override def toString: String = {
    (if(piece != Piece.Pawn) piece else "") +
    Square.toString(source) + 
    (if(moveType == MoveType.Capture || moveType == MoveType.EnPassant) "x" else "") +
    Square.toString(destination) + 
    promotionPiece.foldLeft("") { (_, promPiece) => "=" + promPiece }
  }
}

/** A class of move type.
 * 
 * @author Łukasz Szpakowski
 */
final class MoveType private(val id: Int, val name: String) extends EnumValue

/** A singleton for move type.
 * 
 * @author Łukasz Szpakowski
 */
object MoveType
{
  val NormalMove = new MoveType(0, "NormalMove") 
  val Capture = new MoveType(1, "Capture")
  val EnPassant = new MoveType(2, "EnPassant")
  val KingsideCastling = new MoveType(3, "KingsideCastling")
  val QueensideCastling = new MoveType(4, "QueensideCastling")
  
  val Values = Array(NormalMove, Capture, EnPassant, KingsideCastling, QueensideCastling)
  
  def apply(id: Int): MoveType =
    Values(id)
}

/** A singleton for normal move.
 * 
 * @author Łukasz Szpakowski
 */
object NormalMove
{
  def apply(piece: Piece, src: Int, dst: Int, promPiece: PieceOption): Move =
    Move(piece, src, dst, promPiece, MoveType.NormalMove)
    
  def unapply(move: Move): Option[(Piece, Int, Int, PieceOption)] =
    move match {
      case Move(piece, src, dst, promPiece, MoveType.NormalMove) => Some(piece, src, dst, promPiece) 
      case _                                                     => None
    }
}

/** A capture singleton.
 * 
 * @author Łukasz Szpakowski
 */
object Capture
{
  def apply(piece: Piece, src: Int, dst: Int, promPiece: PieceOption): Move =
    Move(piece, src, dst, promPiece, MoveType.Capture)
    
  def unapply(move: Move): Option[(Piece, Int, Int, PieceOption)] =
    move match {
      case Move(piece, src, dst, promPiece, MoveType.Capture) => Some(piece, src, dst, promPiece) 
      case _                                                  => None
    }
}

/** A singleton for en passant.
 * 
 * @author Łukasz Szpakowski
 */
object EnPassant
{
  def apply(src: Int, dst: Int): Move =
    Move(Piece.Pawn, src, dst, PieceOption.None, MoveType.EnPassant)
    
  def unapply(move: Move): Option[(Int, Int)] =
    move match {
      case Move(Piece.Pawn, src, dst, PieceOption.None, MoveType.EnPassant) => Some(src, dst) 
      case _                                                                => None
    }
}

/** A singleton for kingside castling.
 * 
 * @author Łukasz Szpakowski
 */
object KingsideCastling
{
  def apply(): Move =
    Move(Piece.King, 4, 6, PieceOption.None, MoveType.KingsideCastling)
    
  def unapply(move: Move): Boolean =
    move.moveType == MoveType.KingsideCastling
}

/** A singleton for queenside castling.
 * 
 * @author Łukasz Szpakowski
 */
object QueensideCastling
{
  def apply(): Move =
    Move(Piece.King, 4, 2, PieceOption.None, MoveType.QueensideCastling)
    
  def unapply(move: Move): Boolean =
    move.moveType == MoveType.QueensideCastling
}
