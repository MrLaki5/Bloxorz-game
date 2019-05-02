import board._
import scala.collection.mutable


class MapEditing {

  type BoardType = mutable.ListBuffer[mutable.ListBuffer[Field]]

  def move(direction: Char, x: Int, y: Int, board: BoardType): (Int, Int, Int, Int) = {
    val res = direction match {
      case 'l' => (x, 1, y-1, 1)
      case 'r' => (x, 1, y+2, 1)
      case 'u' => (x-1, 1, y, 1)
      case 'd' => (x+1, 1, y, 1)
      case _ => (-1, 1, -1, 1)
    }
    if(res._1>=0 && res._1<board.length){
      if(res._2>=0 && res._2<board(0).length){
        return res
      }
    }
    (x, 1,y, 1)
  }

  def checkIfSpecific(x: Int, y: Int, board: BoardType, filedType: Char): Boolean = {
    if (x < board.length && x >= 0){
      if(y < board(x).length && y>= 0){
        return board(x)(y).getSign() == filedType
      }
    }
    false
  }

  def isBlockOnEdge(x: Int, y: Int, board: BoardType):Boolean = {
    if(checkIfSpecific(x, y, board, 'O')){
      if(checkIfSpecific(x-1, y, board, '-') || checkIfSpecific(x+1, y, board, '-') || checkIfSpecific(x, y-1, board, '-') || checkIfSpecific(x, y+1, board, '-')){
        return true
      }
    }
    false
  }

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

  def removeBlock(x: Int, y: Int, board: BoardType): Unit = {
    if(isBlockOnEdge(x,y, board)){
      board(x).remove(y)
      board(x).insert(y, new Empty())
    }
  }

  def addBlock(x: Int, y: Int, board: BoardType): Unit = {
    if(isBlockableEdge(x, y, board)){
      board(x).remove(y)
      board(x).insert(y, new Block())
    }
  }

  def addSpecial(x: Int, y: Int, board: BoardType): Unit = {
    if(board(x)(y).getSign() == 'O'){
      board(x).remove(y)
      board(x).insert(y, new Plate())
    }
  }

  def removeSpecial(x: Int, y: Int, board: BoardType): Unit = {
    if(board(x)(y).getSign() == '.'){
      board(x).remove(y)
      board(x).insert(y, new Block())
    }
  }

  def changeStart(x: Int, y: Int, board: BoardType): Unit = {
    if(board(x)(y).getSign() == 'O' || board(x)(y).getSign() == '.'){
      val old_st = findSpecificPosition(board, 'S')
      board(old_st._1).remove(old_st._2)
      board(old_st._1).insert(old_st._2, new Block())
      board(x).remove(y)
      board(x).insert(y, new Start())
    }
  }

  def changeFinish(x: Int, y: Int, board: BoardType): Unit = {
    if(board(x)(y).getSign() == 'O' || board(x)(y).getSign() == '.'){
      val old_st = findSpecificPosition(board, 'T')
      board(old_st._1).remove(old_st._2)
      board(old_st._1).insert(old_st._2, new Block())
      board(x).remove(y)
      board(x).insert(y, new Start())
    }
  }
}