package board

class Start  extends Field {
  override def getSign(): Char = { 'S' }

  override def isWin(): Boolean = { false }

  override def isOut(): Boolean = { false }

  override def isLose(): Boolean = { false }
}
