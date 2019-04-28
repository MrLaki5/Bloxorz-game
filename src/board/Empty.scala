package board

class Empty extends Field {
  override def getSign(): Char = { '-' }

  override def isWin(): Boolean = { false }

  override def isOut(): Boolean = { isSteped() }

  override def isLose(): Boolean = { false }
}
