package mapTools

import scala.collection.mutable


// Class for storing sequence operation
class SequenceOp(val operationName: String, val operationList: mutable.ListBuffer[(Int, Int, MapEditing.BoardType) => (Int, Int, Int, Int)]) {

  // Do operation sequence
  def doOperation(otherSOp: mutable.ListBuffer[SequenceOp])(x: Int, y: Int, board: MapEditing.BoardType): (Int, Int, Int, Int) = {
    var position = (x, 1, y, 1)
    for(operation <- operationList){
      position = operation(position._1, position._3, board)
    }
    position
  }

  // Check name of sequence operation
  def checkName(nameCheck: String): Boolean = {
    nameCheck == operationName
  }
}
