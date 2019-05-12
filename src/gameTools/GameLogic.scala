package gameTools

import java.io.{BufferedWriter, File, FileWriter}
import board._
import scala.collection.mutable
import scala.io.Source


// Logic part that is used for playing the game
object GameLogic {

  type BoardType = mutable.ListBuffer[mutable.ListBuffer[Field]]

  // Checks if position is in game board
  def positionInBounds(x: Int, x_len: Int, y: Int, y_len: Int, board: BoardType): Boolean = {
    val boardLength = board.length
    if (boardLength > 0){
      val boardFieldLength = board.head.length
      if (x<0 || (x+x_len-1)>=boardLength)
        return false
      if (y<0 || (y+y_len-1)>=boardFieldLength)
        return false
      return true
    }
    false
  }

  // Moves from current position to chosen direction
  def move(max_size: Int)(move: Char, x: Int, x_len: Int, y: Int, y_len: Int):(Int, Int, Int, Int) = move match {
    case 'r' if y_len == max_size => (x, x_len, y+2, 1)
    case 'r' if x_len == max_size => (x, x_len, y+1, 1)
    case 'r' => (x, x_len, y+1, max_size)

    case 'l' if y_len == max_size => (x, x_len, y-1, 1)
    case 'l' if x_len == max_size => (x, x_len, y-1, 1)
    case 'l' => (x, x_len, y-2, max_size)

    case 'd' if y_len == max_size => (x+1, 1, y, y_len)
    case 'd' if x_len == max_size => (x+2, 1, y, y_len)
    case 'd' => (x+1, max_size, y, y_len)

    case 'u' if y_len == max_size => (x-1, 1 , y, y_len)
    case 'u' if x_len == max_size => (x-1, 1, y, y_len)
    case 'u' => (x-2, max_size, y, y_len)
  }

  // Writes to board position of player
  def movementWriter(writeData: Boolean, x: Int, x_len: Int, y: Int, y_len: Int, board: BoardType): Boolean = {
    try{
      for(i <- 1 to x_len){ board(x+i-1)(y).setStep(writeData) }
      for(i <- 1 to y_len){ board(x)(y+i-1).setStep(writeData) }
      return true
    }
    catch{
      case _:Throwable =>
    }
    false
  }

  // Gives board state in string
  def boardData(board: BoardType): String = {
    val retData = StringBuilder.newBuilder
    for(row<-board){
      for(data<-row) {
        retData.append(data.toString())
      }
      retData.append("\n")
    }
    retData.toString()
  }

  // Loads new board from string
  def loadBoardFromString(boardStr: String): BoardType = {
    val curr_board = mutable.ListBuffer[mutable.ListBuffer[Field]]()
    curr_board += mutable.ListBuffer[Field]()
    for(char <- boardStr){
      char match {
        case 'o' => curr_board.last += new Block() //From pdf char
        case 'O' => curr_board.last += new Block()
        case '–' => curr_board.last += new Empty() //From pdf char
        case '-' => curr_board.last += new Empty()
        case 'S' => curr_board.last += new Start()
        case 'T' => curr_board.last += new Finish()
        case '.' => curr_board.last += new Plate()
        case '\n' => curr_board += mutable.ListBuffer[Field]()
        case _ =>
      }
    }
    curr_board
  }

  // Loads new board from file
  def loadBoardFromFile(fileName: String): BoardType = {
    val dataStr = Source.fromFile(fileName).mkString
    loadBoardFromString(dataStr)
  }

  // Finds position of start on current board
  def findStartPosition(board: BoardType): (Int, Int, Int, Int) = {
    for(i <- board.indices){
      for(j <- board(i).indices){
        if (board(i)(j).getSign() == 'S'){
          return (i, 1, j, 1)
        }
      }
    }
    throw new RuntimeException("No start found on board")
  }

  // Checks if player has won or lost from current position
  // Returns: 1-Win; 2-Lose; 3-Continue;
  def afterMoveLogic(board: BoardType, x: Int, x_len: Int, y: Int, y_len: Int):Int = {
    val length = x_len * y_len
    if(length == 1){
      if(board(x)(y).isWin()){
        return 1
      }
      if(board(x)(y).isLose()){
        return 2
      }
      if(board(x)(y).isOut()){
        return 2
      }
    }
    else{
      val out_num = (for(i <- board; j <- i; if j.isOut()) yield i).length
      if (out_num * 2 >= length){
        return 2
      }
    }
    3
  }

  // WIN MOVE CALCULATION LOGIC

  // Check if player state on given board position was already played (prevents looping)
  def  moveExists(movesData: mutable.ListBuffer[((Int, Int, Int, Int), String)], move: (Int, Int, Int, Int)): Boolean = {
    for(elem <- movesData; if elem._1 == move) return true
    false
  }

  // Calculates move in specific direction and updates lists of available moves and used moves
  //Returns: 1-Win and win commands; 2-Lose and nothing; 3-Continue and nothing;
  def calculateOneMove(currMove: Char, usedMoves: mutable.ListBuffer[((Int, Int, Int, Int), String)], availableMoves: mutable.ListBuffer[((Int, Int, Int, Int), String)], pos: ((Int, Int, Int, Int), String), board: BoardType): (Int, String) = {
    val currNewPosition = move(2)(currMove, pos._1._1, pos._1._2, pos._1._3, pos._1._4)
    if(!positionInBounds(currNewPosition._1, currNewPosition._2, currNewPosition._3, currNewPosition._4, board)){
      return (2, "")
    }
    val isOkMove = movementWriter(true, currNewPosition._1, currNewPosition._2, currNewPosition._3, currNewPosition._4, board)
    if(!isOkMove){
      movementWriter(false, currNewPosition._1, currNewPosition._2, currNewPosition._3, currNewPosition._4, board)
      return (2, "")
    }
    val currState = afterMoveLogic(board, currNewPosition._1, currNewPosition._2, currNewPosition._3, currNewPosition._4)
    movementWriter(false, currNewPosition._1, currNewPosition._2, currNewPosition._3, currNewPosition._4, board)
    if (currState == 3){
      if (!moveExists(usedMoves, currNewPosition) && !moveExists(availableMoves, currNewPosition)){
        val fullCurrNewPosition = (currNewPosition, pos._2 + currMove)
        availableMoves += fullCurrNewPosition
      }
    }
    else{
      if (currState == 1){
        return (currState, pos._2 + currMove)
      }
    }
    (currState, "")
  }

  // Calculates sequence of moves that are needed for win on given board
  def calculateWinMove(board: BoardType): String = {
    val usedMoves = mutable.ListBuffer[((Int, Int, Int, Int), String)]()
    val availableMoves = mutable.ListBuffer[((Int, Int, Int, Int), String)]()
    val currPosition = findStartPosition(board)
    var fullCurrPosition = (currPosition, "")
    availableMoves += fullCurrPosition
    while (availableMoves.nonEmpty){
      fullCurrPosition = availableMoves.remove(0)
      usedMoves += fullCurrPosition

      val upState = calculateOneMove('u', usedMoves, availableMoves, fullCurrPosition, board)
      if (upState._1 == 1) return upState._2

      val downState = calculateOneMove('d', usedMoves, availableMoves, fullCurrPosition, board)
      if (downState._1 == 1) return downState._2

      val leftState = calculateOneMove('l', usedMoves, availableMoves, fullCurrPosition, board)
      if (leftState._1 == 1) return leftState._2

      val rightState = calculateOneMove('r', usedMoves, availableMoves, fullCurrPosition, board)
      if (rightState._1 == 1) return rightState._2
    }
    ""
  }

  // Save chosen moves sequence to file
  def saveMovesToFile(fileName: String, movesData: String): Unit = {
    val fileFullName = fileName + ".txt"
    val file = new File(fileFullName)
    val bw = new BufferedWriter(new FileWriter(file))
    for(move <- movesData){
      bw.write(move + "\n")
    }
    bw.flush()
    bw.close()
  }

  // Save given board to file
  def saveBoardToFile(fileName: String, board: BoardType): Unit = {
    val fileFullName = fileName + ".txt"
    val file = new File(fileFullName)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(boardData(board))
    bw.flush()
    bw.close()
  }
}
