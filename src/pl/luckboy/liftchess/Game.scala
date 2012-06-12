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

package pl.luckboy.liftchess
import pl.luckboy.liftchess.engine._

/** A class for the chess game.
 * 
 * @author Łukasz Szpakowski
 */
class Game private(
    val tags: Map[String, String], 
    val players: (Option[String], Option[String]),
    val startBoardBuilder: Option[BoardBuilder], 
    val moves: Seq[Move],
    optResult: Option[Result.Value]) 
{
  /** The board builders from the start board to the board after made the last move. */
  val boardBuilders: Seq[BoardBuilder] =
    moves.foldLeft(Seq(startBoardBuilder.getOrElse { BoardBuilder.initialBoardBuilder })) {
      (builders, move) =>
        require(!builders.last.legalMoves.exists(move ==))
        builders :+ BoardBuilder.fromBoard(builders.last.successor(move).get)
    }

  /** The game result. */
  val result: Result.Value = optResult.getOrElse { resultFromMoves }
  
  // Checks whether the game result is correct.
  require(resultFromMoves == Result.Unknown || resultFromMoves == result)
    
  /** Return the optional player name for the specified side.
   * @param side			the side.
   * @return				the optional player name.
   */
  def player(side: Side): Option[String] =
    if(side == Side.White) players._1 else players._2
  
  /** The start board. */
  def startBoard: Option[Board] =
    startBoardBuilder.map { _.toBoard }

  /** The real start board. */
  def realStartBoard: Board =
    startBoard.getOrElse { BoardBuilder.initialBoardBuilder }
  
  /** The boards from the start board to the board after made the last move. */
  def boards: Seq[Board] =
    boardBuilders.view.map { _.toBoard }
  
  /** Returns a move strings. */
  def moveStrings: Seq[String] = throw new Exception  
    
  /** Returns a game result that is calculated from the game moves. */
  def resultFromMoves: Result.Value =
    if(boardBuilders.last.inCheckmate)
      Result.win(boardBuilders.last.side.opposite)
    else if(boardBuilders.last.inStalemate)
      Result.Draw
    else
      Result.Unknown
  
  /** Returns a copy of the chess game with a added  move.
   * @param move		the move.
   * @return			the new chess game.
   */
  def +(move: Move): Game = {
    require(result == Result.Unknown)
    Game(tags, players, startBoardBuilder, moves :+ move, None)
  }
}

/** A singleton for the chess game.
 * 
 * @author Łukasz Szpakowski
 */
object Game
{
  /** Creates a chess game. */
  def apply(
      tags: Map[String, String], 
      players: (Option[String], Option[String]),
      startBoardBuilder: Option[BoardBuilder], 
      moves: Seq[Move],
      optResult: Option[Result.Value]): Game =
    new Game(tags, players, startBoardBuilder, moves, optResult)
}
