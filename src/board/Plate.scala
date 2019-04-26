package board

class Plate  extends Field {
  override def getSign(): Char = { '.' }

  override def isWin(): Boolean = { false }

  override def isOut(): Boolean = { false }

  override def isLose(): Boolean = { true }
}
