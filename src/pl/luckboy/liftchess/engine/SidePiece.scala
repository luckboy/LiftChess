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

/** Class for the side and the piece.
 * 
 * @author Łukasz Szpakowski
 */
final class SidePiece private(val id: Int, val name: String) extends EnumValue
{
  /** Checks whether the side piece is the specified side.
   * @param side		the side.
   * @return			true if it is the specified side.
   */
  def isSide(side: Side): Boolean =
    (id >> 4) == side.id + 1

  /** Checks whether the side piece is the specified piece.
   * @param piece		the piece.
   * @return			true if it is the specified piece.
   */
  def isPiece(piece: Piece): Boolean =
    (id & 15) == piece.id
  
  /** Returns the side. */
  @inline
  def side: Side =
    Side((id >> 4) - 1)

  /** Returns the piece. */
  @inline
  def piece: Piece =
    Piece(id & 15)

  /** Returns the side identifier. */
  @inline
  def sideId: Int =
    (id >> 4) - 1

  /** Return the piece identifier. */
  @inline
  def pieceId: Int =
    id & 15
    
  /** Converts to a optional side piece. */
  def toSidePieceOption: SidePieceOption =
    SidePieceOption(id)
}

/** A Singleton for the side and the piece.
 * 
 * @author Łukasz Szpakowski
 */
object SidePiece
{
  val WhitePawn = new SidePiece(Piece.Pawn.id | 16, "P")
  val WhiteKnight = new SidePiece(Piece.Knight.id | 16, "N")
  val WhiteBishop = new SidePiece(Piece.Bishop.id | 16, "B")
  val WhiteRook = new SidePiece(Piece.Rook.id | 16, "R")
  val WhiteQueen = new SidePiece(Piece.Queen.id | 16, "Q")
  val WhiteKing = new SidePiece(Piece.King.id | 16, "K")

  val BlackPawn = new SidePiece(Piece.Pawn.id | 32, "p")
  val BlackKnight = new SidePiece(Piece.Knight.id | 32, "n")
  val BlackBishop = new SidePiece(Piece.Bishop.id | 32, "b")
  val BlackRook = new SidePiece(Piece.Rook.id | 32, "r")
  val BlackQueen = new SidePiece(Piece.Queen.id | 32, "q")
  val BlackKing = new SidePiece(Piece.King.id | 32, "k")
  
  private val Values = Side.makeArray(
      Piece.makeArray(WhitePawn, WhiteKnight, WhiteBishop, WhiteRook, WhiteQueen, WhiteKing),
      Piece.makeArray(BlackPawn, BlackKnight, BlackBishop, BlackRook, BlackQueen, BlackKing)
      )
  
  /** Creates a side piece from the side and the piece.
   * @param side		the side.
   * @param piece		the piece.
   * @return 			a side piece.
   */
  def fromSideAndPiece(side: Side, piece: Piece): SidePiece =
    Values(side.id)(piece.id)

  /** Creates a side piece from the identifier.
   * @param	id 			the identifier.
   * @return			a side piece.
   */
  def apply(id: Int): SidePiece = 
    Values((id >> 4) - 1)(id & 15)    
  
  /** Converts the side piece to a optional side piece.
   * @param sidePiece	the side piece.
   * @return			the optional side piece.
   */
  implicit def toSidePieceOption(sidePiece: SidePiece): SidePieceOption =
    SidePieceOption(sidePiece.id)
  
  /** Return the set of the side pieces. */
  def values: Set[SidePiece] =
    Values.flatten.toSet
}
