package pl.luckboy.liftchess.engine

final case class GameStateUndo(undo: Undo, repHashKey: Long, numberOfLegalMoves: Int)