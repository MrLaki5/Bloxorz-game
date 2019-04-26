import board.{Block, Field, Start}

import scala.collection.mutable

object test extends App {

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

  val curr_board = mutable.MutableList[mutable.MutableList[Field]]()

  curr_board += mutable.MutableList[Field](new Start(), new Block(), new Block(), new Block())
  curr_board += mutable.MutableList[Field](new Block(), new Block(), new Block(), new Block())
  curr_board += mutable.MutableList[Field](new Block(), new Block(), new Block(), new Block())
  curr_board += mutable.MutableList[Field](new Block(), new Block(), new Block(), new Block())

  //for(i <- test123) print(i.getSign())
  //print(test123(0).getSign())

  var position = (0,1,0,1)
  val moveSet = move(2)(_, _, _, _, _)
  movementWriter(true, position._1, position._2, position._3, position._4, curr_board)
  println(boardData(curr_board))
  while (true){
    val temp = scala.io.StdIn.readLine()
    movementWriter(false, position._1, position._2, position._3, position._4, curr_board)
    position = moveSet(temp.charAt(0), position._1, position._2, position._3, position._4)
    movementWriter(true, position._1, position._2, position._3, position._4, curr_board)
    println(boardData(curr_board))
  }

}
