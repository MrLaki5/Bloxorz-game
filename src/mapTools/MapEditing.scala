package mapTools

import board._
import scala.collection.mutable


// Operations used for map editing
object MapEditing {

  type BoardType = mutable.ListBuffer[mutable.ListBuffer[Field]]

  // Move edit map cursor
  def move(nextF: Option[(Int, Int, BoardType) => (Int, Int, Int, Int)])(direction: Char)(x: Int, y: Int, board: BoardType): (Int, Int, Int, Int) = {
    var res = direction match {
      case 'l' => (x, 1, y-1, 1)
      case 'r' => (x, 1, y+1, 1)
      case 'u' => (x-1, 1, y, 1)
      case 'd' => (x+1, 1, y, 1)
      case _ => (-1, 1, -1, 1)
    }
    if(!(res._1>=0 && res._1<board.length && res._3>=0 && res._3<board.head.length)){
        res = (x, 1, y, 1)
    }
    nextF match {
      case Some(fun) => fun(res._1, res._3, board)
      case None => (res._1, 1, res._3, 1)
    }
  }

  // Check if specific filed is of specific type
  def checkIfSpecific(x: Int, y: Int, board: BoardType, filedType: Char): Boolean = {
    if (x < board.length && x >= 0){
      if(y < board(x).length && y>= 0){
        return board(x)(y).getSign() == filedType
      }
    }
    false
  }

  // Check if specific field is block and if it is on the edge
  def isBlockOnEdge(x: Int, y: Int, board: BoardType):Boolean = {
    if(checkIfSpecific(x, y, board, 'O')){
      if(checkIfSpecific(x-1, y, board, '-') || checkIfSpecific(x+1, y, board, '-') || checkIfSpecific(x, y-1, board, '-') || checkIfSpecific(x, y+1, board, '-')){
        return true
      }
    }
    false
  }

  // Check if specific field is empty and if it is on edge
  def isBlockableEdge(x: Int, y: Int, board: BoardType):Boolean = {
    if(x!=0 && y!=0 && x!=(board.length-1) && y!=(board(0).length-1)){
      if(checkIfSpecific(x, y, board, '-')){
        if(checkIfSpecific(x-1, y, board, 'O') || checkIfSpecific(x+1, y, board, 'O') || checkIfSpecific(x, y-1, board, 'O') || checkIfSpecific(x, y+1, board, 'O')){
          return true
        }
        if(checkIfSpecific(x-1, y, board, 'S') || checkIfSpecific(x+1, y, board, 'S') || checkIfSpecific(x, y-1, board, 'S') || checkIfSpecific(x, y+1, board, 'S')){
          return true
        }
        if(checkIfSpecific(x-1, y, board, 'T') || checkIfSpecific(x+1, y, board, 'T') || checkIfSpecific(x, y-1, board, 'T') || checkIfSpecific(x, y+1, board, 'T')){
          return true
        }
        if(checkIfSpecific(x-1, y, board, '.') || checkIfSpecific(x+1, y, board, '.') || checkIfSpecific(x, y-1, board, '.') || checkIfSpecific(x, y+1, board, '.')){
          return true
        }
      }
    }
    false
  }

  // Find first appearance of specific field on board
  def findSpecificPosition(board: BoardType, findChar: Char): (Int, Int) = {
    for(i <- board.indices){
      for(j <- board(i).indices){
        if (board(i)(j).getSign() == findChar){
          return (i, j)
        }
      }
    }
    throw new RuntimeException("No char found on board")
  }

  // Remove block that is on edge
  def removeBlock(nextF: Option[(Int, Int, BoardType) => (Int, Int, Int, Int)])(x: Int, y: Int, board: BoardType): (Int, Int, Int, Int) = {
    if(isBlockOnEdge(x,y, board)){
      board(x).remove(y)
      board(x).insert(y, new Empty())
      board(x)(y).setStep(true)
    }
    nextF match {
      case Some(fun) => fun(x, y, board)
      case None => (x, 1, y, 1)
    }
  }

  // Add block to edge of board
  def addBlock(nextF: Option[(Int, Int, BoardType) => (Int, Int, Int, Int)])(x: Int, y: Int, board: BoardType): (Int, Int, Int, Int) = {
    if(isBlockableEdge(x, y, board)){
      board(x).remove(y)
      board(x).insert(y, new Block())
      board(x)(y).setStep(true)
    }
    nextF match {
      case Some(fun) => fun(x, y, board)
      case None => (x, 1, y, 1)
    }
  }

  // If specific field is block type change it to special
  def addSpecial(nextF: Option[(Int, Int, BoardType) => (Int, Int, Int, Int)])(x: Int, y: Int, board: BoardType): (Int, Int, Int, Int) = {
    if(board(x)(y).getSign() == 'O'){
      board(x).remove(y)
      board(x).insert(y, new Plate())
      board(x)(y).setStep(true)
    }
    nextF match {
      case Some(fun) => fun(x, y, board)
      case None => (x, 1, y, 1)
    }
  }

  // Remove specific special field and put normal block
  def removeSpecial(nextF: Option[(Int, Int, BoardType) => (Int, Int, Int, Int)])(x: Int, y: Int, board: BoardType): (Int, Int, Int, Int) = {
    if(board(x)(y).getSign() == '.'){
      board(x).remove(y)
      board(x).insert(y, new Block())
      board(x)(y).setStep(true)
    }
    nextF match {
      case Some(fun) => fun(x, y, board)
      case None => (x, 1, y, 1)
    }
  }

  // To specific board field that is block or special put start position
  def changeStart(nextF: Option[(Int, Int, BoardType) => (Int, Int, Int, Int)])(x: Int, y: Int, board: BoardType): (Int, Int, Int, Int) = {
    if(board(x)(y).getSign() == 'O' || board(x)(y).getSign() == '.'){
      val old_st = findSpecificPosition(board, 'S')
      board(old_st._1).remove(old_st._2)
      board(old_st._1).insert(old_st._2, new Block())
      board(x).remove(y)
      board(x).insert(y, new Start())
      board(x)(y).setStep(true)
    }
    nextF match {
      case Some(fun) => fun(x, y, board)
      case None => (x, 1, y, 1)
    }
  }

  // To specific board field that is block or special put finish position
  def changeFinish(nextF: Option[(Int, Int, BoardType) => (Int, Int, Int, Int)])(x: Int, y: Int, board: BoardType): (Int, Int, Int, Int) = {
    if(board(x)(y).getSign() == 'O' || board(x)(y).getSign() == '.'){
      val old_st = findSpecificPosition(board, 'T')
      board(old_st._1).remove(old_st._2)
      board(old_st._1).insert(old_st._2, new Block())
      board(x).remove(y)
      board(x).insert(y, new Finish())
      board(x)(y).setStep(true)
    }
    nextF match {
      case Some(fun) => fun(x, y, board)
      case None => (x, 1, y, 1)
    }
  }

  // On board change places of start and finish position
  def inversion(nextF: Option[(Int, Int, BoardType) => (Int, Int, Int, Int)])(x: Int, y: Int, board: BoardType): (Int, Int, Int, Int) = {
    val old_st = findSpecificPosition(board, 'S')
    val old_fin = findSpecificPosition(board, 'T')
    board(old_st._1).remove(old_st._2)
    board(old_st._1).insert(old_st._2, new Finish())
    board(old_fin._1).remove(old_fin._2)
    board(old_fin._1).insert(old_fin._2, new Start())
    nextF match {
      case Some(fun) => fun(x, y, board)
      case None => (x, 1, y, 1)
    }
  }

  // Remove all special field from board
  def removeAllSpecial(nextF: Option[(Int, Int, BoardType) => (Int, Int, Int, Int)])(x: Int, y: Int, board: BoardType): (Int, Int, Int, Int) = {
    for(i <- board.indices) {
      for (j <- board(i).indices) {
        if (board(i)(j).getSign() == '.') {
          board(i).remove(j)
          board(i).insert(j, new Block())
        }
      }
    }
    nextF match {
      case Some(fun) => fun(x, y, board)
      case None => (x, 1, y, 1)
    }
  }

  // If specific field is special if in range of n fields there is another special field change specific to normal block
  def filter(nextF: Option[(Int, Int, BoardType) => (Int, Int, Int, Int)])(n: Int)(x: Int, y: Int, board: BoardType): (Int, Int, Int, Int) = {
    for(i <- board.indices) {
      for (j <- board(i).indices) {
        if (board(i)(j).getSign() == '.') {
          if(((i-n)<=x && (i+n)>=x) && ((j-n)<=y && (j+n)>=y)){
            if(i!=x || j!=y) {
              board(x).remove(y)
              board(x).insert(y, new Block())
              nextF match {
                case Some(fun) => fun(x, y, board)
                case None => (x, 1, y, 1)
              }
            }
          }
        }
      }
    }
    nextF match {
      case Some(fun) => fun(x, y, board)
      case None => (x, 1, y, 1)
    }
  }

  // Remove player cursor from board
  def removeCursor(board: BoardType): Unit ={
    for(i <- board){
      for(j <- i){
        if(j.isSteped()){
          j.setStep(false)
        }
      }
    }
  }

}
