package mapTools

import board.Field
import gameTools.GameLogic

import scala.collection.mutable


class SequenceOp(val operationName: String, val operationList: mutable.ListBuffer[String], val argumentsList: mutable.ListBuffer[Int]) {

  type BoardType = mutable.ListBuffer[mutable.ListBuffer[Field]]
  val mapEditing = new MapEditing()
  val gameLogic = new GameLogic()

  def doOperation(curr_poss: (Int, Int, Int, Int), board: BoardType, otherSOp: mutable.ListBuffer[SequenceOp]): (Int, Int, Int, Int) = {
    var position = curr_poss
    var argumentNum = 0
    for(operation <- operationList){
      operation match {
        case "move left" =>
          gameLogic.movementWriter(false, position._1, position._2, position._3, position._4, board)
          position = mapEditing.move('l', position._1, position._3, board)
          gameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
        case "move right" =>
          gameLogic.movementWriter(false, position._1, position._2, position._3, position._4, board)
          position = mapEditing.move('r', position._1, position._3, board)
          gameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
        case "move up" =>
          gameLogic.movementWriter(false, position._1, position._2, position._3, position._4, board)
          position = mapEditing.move('u', position._1, position._3, board)
          gameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
        case "move down" =>
          gameLogic.movementWriter(false, position._1, position._2, position._3, position._4, board)
          position = mapEditing.move('d', position._1, position._3, board)
          gameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
        case "add block" =>
          mapEditing.addBlock(position._1, position._3, board)
        case "rm block" =>
          mapEditing.removeBlock(position._1, position._3, board)
        case "add special" =>
          mapEditing.addSpecial(position._1, position._3, board)
        case "rm special" =>
          mapEditing.addSpecial(position._1, position._3, board)
        case "put start" =>
          mapEditing.changeStart(position._1, position._3, board)
        case "put finish" =>
          mapEditing.changeFinish(position._1, position._3, board)
        case "invert" =>
          mapEditing.inversion(board)
          gameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
        case "rm all special" =>
          mapEditing.removeAllSpecial(board)
          gameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
        case "filter" =>
          mapEditing.removeNSpecial(position._1, position._3, argumentsList(argumentNum) ,board)
          gameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
          argumentNum += 1
        case _ =>
          for(sOp <- otherSOp){
            if(sOp.checkName(operation)){
              println(sOp.operationName)
              position = sOp.doOperation(position, board, otherSOp)
            }
          }
      }
    }
    position
  }

  def checkName(nameCheck: String): Boolean = {
    nameCheck == operationName
  }
}
