package board

// Class representing finish position on board
class Finish extends Field {
  override def getSign(): Char = { 'T' }

  override def isWin(): Boolean = { true }

  override def isOut(): Boolean = { false }

  override def isLose(): Boolean = { false }
}
