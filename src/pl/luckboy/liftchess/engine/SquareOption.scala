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

/** A class for the optional square.
 * 
 * @author Łukasz Szpakowski
 */
final class SquareOption private(val id: Int, val name: String) extends EnumValue
{
  def square =
    id
  
  @inline
  def foldLeft[@specialized T](z: T)(f: (T, Int) => T): T = 
    if(id == -1) z else f(z, id)    
}

/** A singleton for the optional square.
 * 
 * @author Łukasz Szpakowski
 */
object SquareOption
{
  val None = new SquareOption(-1, "-")
  
  private val Values = (0 to 63).map { sq => new SquareOption(sq, Square.toString(sq)) }.toArray
  
  def apply(sq: Int) =
    Values(sq)
    
  def values: Set[SquareOption] =
    Values.toSet + None
}
