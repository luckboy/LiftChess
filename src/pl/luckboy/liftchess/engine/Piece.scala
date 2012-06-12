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

/** A piece class.
 * 
 * @author Łukasz Szpakowski
 */
final class Piece private(val id: Int, val name: String) extends EnumValue
{
  def toPieceOption: PieceOption =
    PieceOption(id)
}

/** A piece singleton.
 * 
 * @author Łukasz Szpakowski
 */
object Piece
{
  val Pawn = new Piece(0, "P")
  val Knight = new Piece(1, "N")
  val Bishop = new Piece(2, "B")
  val Rook = new Piece(3, "R")
  val Queen = new Piece(4, "Q")
  val King = new Piece(5, "K")
  
  private val Values = makeArray(Pawn, Knight, Bishop, Rook, Queen, King)
  
  def apply(id: Int): Piece = 
    Values(id)
    
  def makeArray[T](p: T, n: T, b: T, r: T, q: T, k: T)(implicit m: ClassManifest[T]): Array[T] = 
    Array(p, n, b, r, q, k)
    
  implicit def toPieceOption(piece: Piece): PieceOption =
    PieceOption(piece.id)
    
  def values: Set[Piece] =
    Values.toSet
}
