import board._

import scala.collection.mutable
import scala.io.Source

object test extends App {

  def positionInBounds(x: Int, x_len: Int, y: Int, y_len: Int, board: mutable.MutableList[mutable.MutableList[Field]]): Boolean = {
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

  def movementWriter(writeData: Boolean, x: Int, x_len: Int, y: Int, y_len: Int, board: mutable.MutableList[mutable.MutableList
    [Field]]): Unit = {
    for(i <- 1 to x_len){ board(x+i-1)(y).setStep(writeData) }
    for(i <- 1 to y_len){ board(x)(y+i-1).setStep(writeData) }
  }

  def boardData(board: mutable.MutableList[mutable.MutableList[Field]]): String = {
    val retData = StringBuilder.newBuilder
    for(row<-board){
      for(data<-row) {
        retData.append(data.toString())
      }
      retData.append("\n")
    }
    retData.toString()
  }

  def loadBoardFromString(boardStr: String): mutable.MutableList[mutable.MutableList[Field]] = {
    val curr_board = mutable.MutableList[mutable.MutableList[Field]]()
    curr_board += mutable.MutableList[Field]()
    for(char <- boardStr){
      char match {
        case 'o' => curr_board.last += new Block() //From pdf char
        case 'O' => curr_board.last += new Block()
        case '–' => curr_board.last += new Empty() //From pdf char
        case '-' => curr_board.last += new Empty()
        case 'S' => curr_board.last += new Start()
        case 'T' => curr_board.last += new Finish()
        case '.' => curr_board.last += new Plate()
        case '\n' => curr_board += mutable.MutableList[Field]()
        case _ =>
      }
    }
    curr_board
  }

  def loadBoardFromFile(fileName: String): mutable.MutableList[mutable.MutableList[Field]] = {
    val dataStr = Source.fromFile(fileName).mkString
    loadBoardFromString(dataStr)
  }

  def findStartPosition(board: mutable.MutableList[mutable.MutableList[Field]]): (Int, Int, Int, Int) = {
    for(i <- board.indices){
      for(j <- board(i).indices){
        if (board(i)(j).getSign() == 'S'){
          return (i, 1, j, 1)
        }
      }
    }
    throw new RuntimeException("No start found on board")
  }

  // 1-Win; 2-Lose; 3-Continue
  def afterMoveLogic(board: mutable.MutableList[mutable.MutableList[Field]], x: Int, x_len: Int, y: Int, y_len: Int):Int = {
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

  def  moveExists(movesData: mutable.ListBuffer[((Int, Int, Int, Int), String)], move: (Int, Int, Int, Int)): Boolean = {
    for(elem <- movesData; if elem._1 == move) return true
    false
  }

  def calculateOneMove(currMove: Char, usedMoves: mutable.ListBuffer[((Int, Int, Int, Int), String)], availableMoves: mutable.ListBuffer[((Int, Int, Int, Int), String)], pos: ((Int, Int, Int, Int), String), board: mutable.MutableList[mutable.MutableList[Field]]): (Int, String) = {
    val currNewPosition = move(2)(currMove, pos._1._1, pos._1._2, pos._1._3, pos._1._4)
    if(!positionInBounds(currNewPosition._1, currNewPosition._2, currNewPosition._3, currNewPosition._4, board)){
      return (2, "")
    }
    movementWriter(true, currNewPosition._1, currNewPosition._2, currNewPosition._3, currNewPosition._4, board)
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

  def calculateWinMove(board: mutable.MutableList[mutable.MutableList[Field]]): String = {
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

  val moveSet = move(2)(_, _, _, _, _)

  val test_moves_board = loadBoardFromFile("map1.txt")
  val retMoves = calculateWinMove(test_moves_board)
  println(retMoves)

  val curr_board = loadBoardFromFile("map1.txt")

  var position = findStartPosition(curr_board)
  movementWriter(true, position._1, position._2, position._3, position._4, curr_board)
  println(boardData(curr_board))
  var state = 3
  while (state == 3){
    val temp = scala.io.StdIn.readLine()
    movementWriter(false, position._1, position._2, position._3, position._4, curr_board)
    position = moveSet(temp.charAt(0), position._1, position._2, position._3, position._4)
    movementWriter(true, position._1, position._2, position._3, position._4, curr_board)
    println(boardData(curr_board))
    state = afterMoveLogic(curr_board, position._1, position._2, position._3, position._4)
  }

  if(state == 1){
    println("Win, congratulation!")
  }
  else{
    println("You lose!")
  }

}
