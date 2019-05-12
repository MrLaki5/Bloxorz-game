package mapTools

import board.Field
import gameTools.GameLogic
import scala.collection.mutable


// Class for storing sequence operation
class SequenceOp(val operationName: String, val operationList: mutable.ListBuffer[String], val argumentsList: mutable.ListBuffer[Int]) {

  type BoardType = mutable.ListBuffer[mutable.ListBuffer[Field]]

  // Do operation sequence
  def doOperation(curr_poss: (Int, Int, Int, Int), board: BoardType, otherSOp: mutable.ListBuffer[SequenceOp]): (Int, Int, Int, Int) = {
    var position = curr_poss
    var argumentNum = 0
    for(operation <- operationList){
      operation match {
        case "move left" =>
          GameLogic.movementWriter(false, position._1, position._2, position._3, position._4, board)
          position = MapEditing.move(None)('l')(position._1, position._3, board)
          GameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
        case "move right" =>
          GameLogic.movementWriter(false, position._1, position._2, position._3, position._4, board)
          position = MapEditing.move(None)('r')(position._1, position._3, board)
          GameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
        case "move up" =>
          GameLogic.movementWriter(false, position._1, position._2, position._3, position._4, board)
          position = MapEditing.move(None)('u')(position._1, position._3, board)
          GameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
        case "move down" =>
          GameLogic.movementWriter(false, position._1, position._2, position._3, position._4, board)
          position = MapEditing.move(None)('d')(position._1, position._3, board)
          GameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
        case "add block" =>
          MapEditing.addBlock(None)(position._1, position._3, board)
        case "rm block" =>
          MapEditing.removeBlock(None)(position._1, position._3, board)
        case "add special" =>
          MapEditing.addSpecial(None)(position._1, position._3, board)
        case "rm special" =>
          MapEditing.addSpecial(None)(position._1, position._3, board)
        case "put start" =>
          MapEditing.changeStart(None)(position._1, position._3, board)
        case "put finish" =>
          MapEditing.changeFinish(None)(position._1, position._3, board)
        case "invert" =>
          MapEditing.inversion(None)(position._1, position._3, board)
          GameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
        case "rm all special" =>
          MapEditing.removeAllSpecial(None)(position._1, position._3, board)
          GameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
        case "filter" =>
          MapEditing.filter(None)(argumentsList(argumentNum))(position._1, position._3 ,board)
          GameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
          argumentNum += 1
        case _ =>
          for(sOp <- otherSOp){
            if(sOp.checkName(operation)){
              position = sOp.doOperation(position, board, otherSOp)
            }
          }
      }
    }
    position
  }

  // Check name of sequence operation
  def checkName(nameCheck: String): Boolean = {
    nameCheck == operationName
  }
}
