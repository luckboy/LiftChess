package pl.luckboy.liftchess.engine

case class Undo(move: Move, data: Int, enPassant: SquareOption, halfmoveClock: Int, hashKey: Long)