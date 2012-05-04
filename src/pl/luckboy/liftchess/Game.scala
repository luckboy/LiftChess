package pl.luckboy.liftchess
import pl.luckboy.liftchess.engine._

/** A class for the chess game.
 * 
 * @author Łukasz Szpakowski
 */
class Game private(
    val tags: Map[String, String], 
    val startBoardBuilder: BoardBuilder, 
    val moves: Seq[Move],
    val result: Result.Value) 
{
  /** The board builders from the start board to the board after made the last move. */
  val boardBuilders: Seq[BoardBuilder] =
    moves.foldLeft(Seq(startBoardBuilder)) {
      (builders, move) =>
        require(!builders.last.legalMoves.exists(move ==))
        builders :+ BoardBuilder.fromBoard(builders.last.successor(move).get)
    }
  
  /** The start board builder. */
  def startBoard: Board =
    startBoardBuilder
  
  /** The boards from the start board to the board after made the last move. */
  def boards: Seq[Board] =
    boardBuilders.map { _.toBoard }
  
  /** Returns a move strings. */
  def moveStrings: Seq[String] = throw new Exception
}