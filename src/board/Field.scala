package board

// Abstract class representing field of board
abstract class Field(var step:Boolean = false) {
  def getSign():Char

  def isWin(): Boolean

  def isOut(): Boolean

  def isLose(): Boolean

  def isSteped(): Boolean = { step }

  def setStep(st: Boolean) = { step = st }

  override def toString(): String = {
    if (isSteped()) "B"
    else getSign().toString
  }
}
