package board

class Block extends Field {
  override def isWin(): Boolean = { false }

  override def isOut(): Boolean = { false }

  override def isLose(): Boolean = { false }

  override def getSign(): Char = { 'O' }
}
