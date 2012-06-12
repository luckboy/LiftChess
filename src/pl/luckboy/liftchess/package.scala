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

package pl.luckboy
import pl.luckboy.liftchess.engine._

/**
 * @author Łukasz Szpakowski
 */
package object liftchess 
{
  //
  // The implicit conversions for PieceOption.
  //
  
  implicit def pieceOptionToOption(pieceOption: PieceOption): Option[Piece] =
    pieceOption.foldLeft(None: Option[Piece]) { (_, piece) => Some(piece) }

  implicit def pieceOptionToIterable(pieceOption: PieceOption): Iterable[Piece] =
    pieceOptionToOption(pieceOption)
  
  implicit def optionToPieceOption(option: Option[Piece]): PieceOption = 
    option.foldLeft(PieceOption.None) { (_, piece) => piece }

  //
  // The implicit conversions for SideOption.
  //
    
  implicit def sideOptionToOption(sideOption: SideOption): Option[Side] =
    sideOption.foldLeft(None: Option[Side]) { (_, side) => Some(side) }

  implicit def sideToIterable(sideOption: SideOption): Iterable[Side] =
    sideOptionToOption(sideOption)
  
  implicit def optionToSideOption(option: Option[Side]): SideOption =
    option.foldLeft(SideOption.None) { (_, side) => side }
  
  //
  // The implicit conversions for SidePieceOption.
  //

  implicit def sidePieceOptionToOption(sidePieceOption: SidePieceOption): Option[SidePiece] =
    sidePieceOption.foldLeft(None: Option[SidePiece]) { (_, sidePiece) => Some(sidePiece) }
  
  implicit def sidePieceOptionToIterable(sidePieceOption: SidePieceOption): Iterable[SidePiece] =
    sidePieceOptionToOption(sidePieceOption)
  
  implicit def optionToSidePieceOption(option: Option[SidePiece]): SidePieceOption =
    option.foldLeft(SidePieceOption.None) { (_, sidePiece) => sidePiece }
  
  implicit def sidePieceOptionToTuple2Option(sidePieceOption: SidePieceOption): Option[(Side, Piece)] =
    sidePieceOption.foldLeft(None: Option[(Side, Piece)]) { (_, sidePiece) => Some(sidePiece) }
  
  implicit def sidePieceOptionToTuple2Iterable(sidePieceOption: SidePieceOption): Iterable[(Side, Piece)] =
    sidePieceOptionToTuple2Option(sidePieceOption)

  implicit def tuple2OptionToSidePieceOption(option: Option[(Side, Piece)]): SidePieceOption =
    option.foldLeft(SidePieceOption.None) { (_, sidePiece) => sidePiece }

  //
  // The implicit conversions for SidePiece.
  //
  
  implicit def sidePieceToTuple2(sidePiece: SidePiece): (Side, Piece) =
    (sidePiece.side, sidePiece.piece)
    
  implicit def tuple2ToSidePiece(p: (Side, Piece)): SidePiece =
    SidePiece.fromSideAndPiece(p._1, p._2)
    
  //
  // The implicit conversions for SquareOption.
  //
    
  implicit def squareOptionToOption(sqOption: SquareOption): Option[Int] =
    sqOption.foldLeft(None: Option[Int]) { (_, sq) => Some(sq) }

  implicit def squareOptionToIterable(sqOption: SquareOption): Iterable[Int] =
    squareOptionToOption(sqOption)
    
  implicit def optionToSquareOption(option: Option[Int]): SquareOption =
    option.foldLeft(SquareOption.None) { (_, sq) => SquareOption(sq) }
}
