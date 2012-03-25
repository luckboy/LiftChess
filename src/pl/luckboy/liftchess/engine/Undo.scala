package pl.luckboy.liftchess.engine

final case class Undo(move: Move, data: Int, enPassant: SquareOption, halfmoveClock: Int, hashKey: Long)