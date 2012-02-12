package pl.luckboy.liftchess.engine

case class Move(piece: Piece.Value, source: Int, destination: Int, promotion: Piece.Value)
