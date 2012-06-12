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

/** A class for the optional piece.
 * 
 * @author Łukasz Szpakowski
 */
final class PieceOption private(val id: Int, val name: String) extends EnumValue
{
  @inline
  def foldLeft[@specialized T](z: T)(f: (T, Piece) => T): T =
    if(id != 6) f(z, Piece(id)) else z
}

/** A singleton for the optional piece.
 * 
 * @author Łukasz Szpakowski
 */
object PieceOption
{
  val None = new PieceOption(6, "_")
  val Pawn = new PieceOption(Piece.Pawn.id, Piece.Pawn.name)
  val Knight = new PieceOption(Piece.Knight.id, Piece.Knight.name)
  val Bishop = new PieceOption(Piece.Bishop.id, Piece.Bishop.name)
  val Rook = new PieceOption(Piece.Rook.id, Piece.Rook.name)
  val Queen = new PieceOption(Piece.Queen.id, Piece.Queen.name)
  val King = new PieceOption(Piece.King.id, Piece.King.name)
  
  private val Values = makeArray(Pawn, Knight, Bishop, Rook, Queen, King, None)
  
  def apply(id: Int): PieceOption = 
    Values(id)
    
  def makeArray[T](p: T, n: T, b: T, r: T, q: T, k: T, none: T)(implicit m: ClassManifest[T]): Array[T] =
    Piece.makeArray(p, n, b, r, q, k) :+ none
    
  def values: Set[PieceOption] =
    Values.toSet
}
