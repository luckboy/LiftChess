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

/** A side class.
 * 
 * @author Łukasz Szpakowski
 */
final class Side private(val id: Int, val name: String) extends EnumValue
{
  /** Returns the opposite side. */
  @inline
  def opposite: Side =
    Side(id ^ 1)
    
  /** Converts to a side option. */
  def toSideOption: SideOption =
    SideOption(id)
}

/** A side singleton.
 * 
 * @author Łukasz Szpakowski
 */
object Side
{
  val White = new Side(0, "w")
  val Black = new Side(1, "b")
  
  private val Values = makeArray(White, Black)
  
  def apply(id: Int): Side =
    Values(id)
    
  def makeArray[T](w: T, b: T)(implicit m: ClassManifest[T]) =
    Array(w, b)
    
  implicit def toSideOption(side: Side): SideOption =
    SideOption(side.id)
    
  def values: Set[Side] =
    Values.toSet
}
