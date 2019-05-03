import scala.swing.Panel
import java.awt.{Color, Graphics2D}

import board.Field

import scala.collection.mutable

class BoardCanvas extends Panel {

  type BoardType = mutable.ListBuffer[mutable.ListBuffer[Field]]

  var board: BoardType = _
  val boardSem = "boardSem"

  var gameText = ""

  override def paintComponent(g: Graphics2D) {

    // Start by erasing this Canvas
    g.clearRect(0, 0, size.width, size.height)

    // Get board for drawing
    var drawBoard: BoardType = null
    var drawText: String = ""
    synchronized(board){
      drawBoard = board
      drawText = gameText
      1
    }

    if(drawBoard != null){
      // Calculate proportion
      val proportion = drawBoard.length/drawBoard.head.length
      // Calculate size of board
      var big_num: Double = 0
      var small_num: Double = 0
      if(size.width>size.height){
        big_num = size.width
        small_num = size.height
      }
      else{
        big_num = size.height
        small_num = size.width
      }
      var logicStatement = true
      while(logicStatement){
        val temp = big_num * proportion
        if(temp <= small_num){
          small_num = temp
          logicStatement = false
        }
        else{
          big_num = big_num * 0.95
        }
      }
      var new_w: Double = 0
      var new_h: Double = 0
      if(size.width>size.height){
        new_w = big_num
        new_h = small_num
      }
      else{
        new_h = big_num
        new_w = small_num
      }
      // Calculate padding
      val pad_w = (size.width - new_w) / 2
      val pad_h = (size.height - new_h) / 2
      // Block sizes
      val big_block_size = (new_w / drawBoard.head.length).toInt
      val small_block_size = big_block_size * 0.90
      var x_cord = 0
      var y_cord = 0
      for(i<-drawBoard){
        for(j<-i){
          val color = j.toString() match {
            case "O" => Color.GRAY
            case "." => Color.DARK_GRAY
            case "B" => Color.GREEN
            case "S" => Color.WHITE
            case "T" => Color.RED
            case "-" => Color.BLACK
            case _ => Color.PINK
          }
          g.setColor(color)
          g.fillRect(x_cord, y_cord, big_block_size, big_block_size)
          x_cord += big_block_size
        }
        x_cord = 0
        y_cord += big_block_size
      }
      if(drawText != ""){
        g.drawString(drawText, size.width/2, size.height/2)
      }
    }
  }

  /** Add a "dart" to list of things to display */
  def setBoard(curr_board: BoardType) {
    synchronized(boardSem){
      board = curr_board
      1
    }
    // Tell Scala that the display should be repainted
    repaint()
  }

  def setText(text: String): Unit = {
    synchronized(boardSem){
      gameText = text
      1
    }
  }
}