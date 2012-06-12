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

/** A class for the castling. 
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

/** A class for the castling.
 * 
 * @author Łukasz Szpakowski
 */
object Castling
{
  val NoneCastling = new Castling(0, "-")
  val KingsideCastling = new Castling(1, "K")
  val QueensideCastling = new Castling(2, "Q")
  val AllCastling = new Castling(3, "KQ")
  
  private val Values = makeArray(NoneCastling, KingsideCastling, QueensideCastling, AllCastling)
  
  def apply(id: Int): Castling =
    Values(id)
    
  def makeArray[T](none: T, k: T, q: T, kq: T)(implicit m: ClassManifest[T]) =
    Array(none, k, q, kq)
    
  def values: Set[Castling] =
    Values.toSet
}
