package pl.luckboy.liftchess.engine

/**
 * @author Łukasz Szpakowski
 */
case class Move(piece: Piece, source: Int, destination: Int, promotion: PieceOption, flags: Int)