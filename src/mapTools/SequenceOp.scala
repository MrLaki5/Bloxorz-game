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
          position = mapEditing.move(0, Array[Option[(Int, Int, BoardType) => (Int, Int, Int, Int)]](None))('l')(position._1, position._3, board)
          gameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
        case "move right" =>
          gameLogic.movementWriter(false, position._1, position._2, position._3, position._4, board)
          position = mapEditing.move(0, Array[Option[(Int, Int, BoardType) => (Int, Int, Int, Int)]](None))('r')(position._1, position._3, board)
          gameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
        case "move up" =>
          gameLogic.movementWriter(false, position._1, position._2, position._3, position._4, board)
          position = mapEditing.move(0, Array[Option[(Int, Int, BoardType) => (Int, Int, Int, Int)]](None))('u')(position._1, position._3, board)
          gameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
        case "move down" =>
          gameLogic.movementWriter(false, position._1, position._2, position._3, position._4, board)
          position = mapEditing.move(0, Array[Option[(Int, Int, BoardType) => (Int, Int, Int, Int)]](None))('d')(position._1, position._3, board)
          gameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
        case "add block" =>
          mapEditing.addBlock(0, Array[Option[(Int, Int, BoardType) => (Int, Int, Int, Int)]](None))(position._1, position._3, board)
        case "rm block" =>
          mapEditing.removeBlock(0, Array[Option[(Int, Int, BoardType) => (Int, Int, Int, Int)]](None))(position._1, position._3, board)
        case "add special" =>
          mapEditing.addSpecial(0, Array[Option[(Int, Int, BoardType) => (Int, Int, Int, Int)]](None))(position._1, position._3, board)
        case "rm special" =>
          mapEditing.addSpecial(0, Array[Option[(Int, Int, BoardType) => (Int, Int, Int, Int)]](None))(position._1, position._3, board)
        case "put start" =>
          mapEditing.changeStart(0, Array[Option[(Int, Int, BoardType) => (Int, Int, Int, Int)]](None))(position._1, position._3, board)
        case "put finish" =>
          mapEditing.changeFinish(0, Array[Option[(Int, Int, BoardType) => (Int, Int, Int, Int)]](None))(position._1, position._3, board)
        case "invert" =>
          mapEditing.inversion(0, Array[Option[(Int, Int, BoardType) => (Int, Int, Int, Int)]](None))(position._1, position._3, board)
          gameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
        case "rm all special" =>
          mapEditing.removeAllSpecial(0, Array[Option[(Int, Int, BoardType) => (Int, Int, Int, Int)]](None))(position._1, position._3, board)
          gameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
        case "filter" =>
          mapEditing.filter(0, Array[Option[(Int, Int, BoardType) => (Int, Int, Int, Int)]](None))(argumentsList(argumentNum))(position._1, position._3 ,board)
          gameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
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

  def checkName(nameCheck: String): Boolean = {
    nameCheck == operationName
  }
}
