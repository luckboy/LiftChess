package pl.luckboy
import pl.luckboy.liftchess.engine._

package object liftchess 
{
  implicit def pieceOptionToOption(pieceOption: PieceOption): Option[Piece] =
    pieceOption.foldLeft(None: Option[Piece]) { (_, piece) => Some(piece) }

  implicit def optionToPieceOption(option: Option[Piece]): PieceOption = 
    option.foldLeft(PieceOption.None) { (_, piece) => piece }
    
  implicit def sideToSideOption(sideOption: SideOption): Option[Side] =
    sideOption.foldLeft(None: Option[Side]) { (_, side) => Some(side) }
  
  implicit def optionToSideOption(option: Option[Side]): SideOption =
    option.foldLeft(SideOption.None) { (_, side) => side }
  
  implicit def sidePieceOptionToOption(sidePieceOption: SidePieceOption): Option[SidePiece] =
    sidePieceOption.foldLeft(None: Option[SidePiece]) { (_, sidePiece) => Some(sidePiece) }
  
  implicit def optionToSidePieceOption(option: Option[SidePiece]): SidePieceOption =
    option.foldLeft(SidePieceOption.None) { (_, sidePiece) => sidePiece }
  
  implicit def sidePieceToTuple2(sidePiece: SidePiece): (Side, Piece) =
    (sidePiece.side, sidePiece.piece)
    
  implicit def Tuple2ToSidePiece(p: (Side, Piece)): SidePiece =
    SidePiece.fromSideAndPiece(p._1, p._2)

  implicit def sidePieceOptionToTuple2Option(sidePieceOption: SidePieceOption): Option[(Side, Piece)] =
    sidePieceOption.foldLeft(None: Option[(Side, Piece)]) { (_, sidePiece) => Some(sidePiece) }
  
  implicit def tuple2OptionToSidePieceOption(option: Option[(Side, Piece)]): SidePieceOption =
    option.foldLeft(SidePieceOption.None) { (_, sidePiece) => sidePiece }
}