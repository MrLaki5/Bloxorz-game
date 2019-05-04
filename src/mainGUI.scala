import java.awt.Color

import board.Field
import javax.swing.{JFileChooser, JFrame}

import scala.collection.mutable
import scala.io.Source
import scala.swing.event.ButtonClicked
import scala.swing.{BorderPanel, Button, Component, Dialog, Dimension, FileChooser, Frame, GridPanel, Label, MainFrame, Panel, SimpleSwingApplication, TextComponent, TextField}

object mainGUI extends SimpleSwingApplication {

  type BoardType = mutable.ListBuffer[mutable.ListBuffer[Field]]
  val gameLogic = new GameLogic()
  val mapEditing = new MapEditing()
  var board: BoardType = _
  var position: (Int, Int, Int, Int) = _
  val moveBuffer = mutable.ListBuffer[String]()


  def top= new MainFrame {
    title = "Bloxorz-game"
    preferredSize = new Dimension(640, 480)
    resizable = false

    // Main menu items
    val mainMenuStartGameButton = new Button {
      text = "Play"
      foreground = Color.blue
      background = Color.red
      borderPainted = true
    }
    val mainMenuCalculateButton = new Button {
      text = "Calculate win sequence"
      foreground = Color.blue
      background = Color.red
      borderPainted = true
    }
    val mainMenuCreateMapButton = new Button {
      text = "Create map"
      foreground = Color.blue
      background = Color.red
      borderPainted = true
    }
    val mainMenuExitButton = new Button {
      text = "Exit"
      foreground = Color.blue
      background = Color.red
      borderPainted = true
    }
    val mainMenuGrid =  new GridPanel(4, 3) {
      contents += new Label("")
      contents += mainMenuStartGameButton
      contents += new Label("")
      contents += new Label("")
      contents += mainMenuCalculateButton
      contents += new Label("")
      contents += new Label("")
      contents += mainMenuCreateMapButton
      contents += new Label("")
      contents += new Label("")
      contents += mainMenuExitButton
      contents += new Label("")
    }
    mainMenuGrid.visible = true
    listenTo(mainMenuStartGameButton)
    listenTo(mainMenuCalculateButton)
    listenTo(mainMenuCreateMapButton)
    listenTo(mainMenuExitButton)
    // Game board items
    val gameLeftButton = new Button {
      text = "Left"
      foreground = Color.blue
      background = Color.red
      borderPainted = true
    }
    val gameRightButton = new Button {
      text = "Right"
      foreground = Color.blue
      background = Color.red
      borderPainted = true
    }
    val gameUpButton = new Button {
      text = "Up"
      foreground = Color.blue
      background = Color.red
      borderPainted = true
    }
    val gameDownButton = new Button {
      text = "Down"
      foreground = Color.blue
      background = Color.red
      borderPainted = true
    }
    val gameFileButton = new Button {
      text = "File"
      foreground = Color.blue
      background = Color.red
      borderPainted = true
    }
    val gameQuitButton = new Button {
      text = "Quit"
      foreground = Color.blue
      background = Color.red
      borderPainted = true
    }
    val gameButtonsGrid =  new GridPanel(1, 6) {
      contents += gameLeftButton
      contents += gameRightButton
      contents += gameUpButton
      contents += gameDownButton
      contents += gameFileButton
      contents += gameQuitButton
    }
    val canvas = new BoardCanvas
    val gameFinishButton = new Button {
      text = "Main menu"
      foreground = Color.blue
      background = Color.red
      borderPainted = true
    }
    val labelFin = new Label{
      text = "Finished!"
    }
    val gameFinishGrid =  new GridPanel(2, 1) {
      contents += labelFin
      contents += gameFinishButton
    }
    listenTo(gameLeftButton)
    listenTo(gameRightButton)
    listenTo(gameUpButton)
    listenTo(gameDownButton)
    listenTo(gameFinishButton)
    listenTo(gameQuitButton)
    listenTo(gameFileButton)
    // Calculate move sequence items
    val moveSequenceStatus = new Label("")
    val moveSequenceGenerateButton = new Button {
      text = "Generate"
      foreground = Color.blue
      background = Color.red
      borderPainted = true
    }
    val moveSequenceBackButton = new Button {
      text = "Back"
      foreground = Color.blue
      background = Color.red
      borderPainted = true
    }
    val moveSequenceGrid = new GridPanel(4, 3){
      contents += new Label("")
      contents += moveSequenceStatus
      contents += new Label("")
      contents += new Label("")
      contents += moveSequenceGenerateButton
      contents += new Label("")
      contents += new Label("")
      contents += moveSequenceBackButton
      contents += new Label("")
    }
    listenTo(moveSequenceGenerateButton)
    listenTo(moveSequenceBackButton)

    contents = mainMenuGrid

    // react to events
    reactions += {
      case ButtonClicked(component: Component) if component == mainMenuExitButton =>
        quit()
      case ButtonClicked(component) if component == mainMenuStartGameButton =>
        val fileName = chooseFile()
        if(fileName != ""){
          try{
            board = gameLogic.loadBoardFromFile(fileName)
            position = gameLogic.findStartPosition(board)
            gameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
            contents = new BorderPanel {
              layout(canvas) = BorderPanel.Position.Center
              layout(gameButtonsGrid) = BorderPanel.Position.South
            }
            canvas.setBoard(board)
          }
          catch {
            case e: Throwable => println(e)
          }
        }
      case ButtonClicked(component) if component == gameLeftButton || component == gameRightButton || component == gameDownButton || component == gameUpButton =>
        var moveCh = 'a'
        if (component==gameLeftButton){
          moveCh = 'l'
        }
        else{
          if (component==gameRightButton){
            moveCh = 'r'
          }
          else{
            if(component==gameDownButton){
              moveCh = 'd'
            }
            else{
              moveCh = 'u'
            }
          }
        }
        gameLogic.movementWriter(false, position._1, position._2, position._3, position._4, board)
        position = gameLogic.move(2)(moveCh, position._1, position._2, position._3, position._4)
        gameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
        canvas.setBoard(board)
        val state = gameLogic.afterMoveLogic(board, position._1, position._2, position._3, position._4)
        if(state==2){
          labelFin.text = "You lose!"

          contents = new BorderPanel {
            layout(canvas) = BorderPanel.Position.Center
            layout(gameFinishGrid) = BorderPanel.Position.South
          }
        }
        if(state==1){
          labelFin.text = "Victory!"
          contents = new BorderPanel {
            layout(canvas) = BorderPanel.Position.Center
            layout(gameFinishGrid) = BorderPanel.Position.South
          }
        }
      case ButtonClicked(component) if component == gameFinishButton || component == moveSequenceBackButton=>
        contents = mainMenuGrid
        mainMenuGrid.repaint()
      case ButtonClicked(component) if component == gameQuitButton =>
        var threadWasAlive = false
        synchronized(threadSemaphore){
          threadWasAlive = threadIsActive
          threadIsActive = false
          0
        }
        if (threadWasAlive){
          moveThread.join()
        }
        contents = mainMenuGrid
        mainMenuGrid.repaint()
      case ButtonClicked(component) if component == gameFileButton =>
        val fileName = chooseFile()
        if(fileName != ""){
          val dataStr = Source.fromFile(fileName).mkString
          for(char <- dataStr){
            char match {
              case 'l' => moveBuffer += "l"
              case 'r' => moveBuffer += "r"
              case 'd' => moveBuffer += "d"
              case 'u' => moveBuffer += "u"
              case '\n' =>
              case _ =>
            }
          }
          moveRun.setFields(canvas, setState, activateButtons)
          synchronized(threadSemaphore){
            threadIsActive = true
            0
          }
          gameLeftButton.enabled = false
          gameRightButton.enabled = false
          gameUpButton.enabled = false
          gameDownButton.enabled = false
          gameFileButton.enabled = false
          moveThread = new Thread(moveRun)
          moveThread.start()
        }
      case ButtonClicked(component) if component == mainMenuCalculateButton =>
        val fileName = chooseFile()
        if(fileName != ""){
          try{
            board = gameLogic.loadBoardFromFile(fileName)
            moveSequenceGenerateButton.enabled = true
            moveSequenceStatus.text = ""
            contents = moveSequenceGrid
            moveSequenceGrid.repaint()
          }
          catch {
            case e: Throwable => println(e)
          }
        }
      case ButtonClicked(component) if component == moveSequenceGenerateButton =>
        val fileName = saveFile()
        if(fileName != ""){
          val retMoves = gameLogic.calculateWinMove(board)
          moveSequenceGenerateButton.enabled = false
          if (retMoves != "") {
            gameLogic.saveMovesToFile(fileName, retMoves)
            moveSequenceStatus.text = "Moves calculated: " + retMoves
          }
          else{
            moveSequenceStatus.text = "There is no win sequence."
          }
        }
    }

    def setState(state: Int): Unit = {
      if(state==2){
        moveBuffer.clear()
        labelFin.text = "You lose!"
        contents = new BorderPanel {
          layout(canvas) = BorderPanel.Position.Center
          layout(gameFinishGrid) = BorderPanel.Position.South
        }
      }
      if(state==1){
        moveBuffer.clear()
        labelFin.text = "Victory!"
        contents = new BorderPanel {
          layout(canvas) = BorderPanel.Position.Center
          layout(gameFinishGrid) = BorderPanel.Position.South
        }
      }
    }

    def activateButtons(): Unit = {
      gameLeftButton.enabled = true
      gameRightButton.enabled = true
      gameUpButton.enabled = true
      gameDownButton.enabled = true
      gameFileButton.enabled = true
    }

    override def closeOperation(): Unit = {
      var threadWasAlive = false
      synchronized(threadSemaphore){
        threadWasAlive = threadIsActive
        threadIsActive = false
        0
      }
      if (threadWasAlive){
        moveThread.join()
      }
      super.closeOperation()
    }

  }

  val moveRun = new moveRunnable()
  var moveThread: Thread = null
  var threadIsActive = false
  val threadSemaphore = "threadSem"

  class moveRunnable extends Runnable{
    var localCanvas: BoardCanvas = _
    var localFinFun: Int=>Unit = _
    var localAfterFun: ()=>Unit = _
    def setFields(canvas: BoardCanvas, finFun: Int=>Unit, afterFun: ()=>Unit): Unit = {
      localCanvas = canvas
      localFinFun = finFun
      localAfterFun = afterFun
    }
    override def run(): Unit = {
      while(moveBuffer.nonEmpty){
        var isActive: Boolean = true
        synchronized(threadSemaphore){
          isActive = threadIsActive
          0
        }
        if(!isActive){
          moveBuffer.clear()
          localAfterFun()
          println("Thread finished by quit")
          return
        }
        val tempCommand = moveBuffer.remove(0)
        gameLogic.movementWriter(false, position._1, position._2, position._3, position._4, board)
        position = gameLogic.move(2)(tempCommand.charAt(0), position._1, position._2, position._3, position._4)
        gameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
        localCanvas.setBoard(board)
        val state = gameLogic.afterMoveLogic(board, position._1, position._2, position._3, position._4)
        localFinFun(state)
        if(state==3){
          Thread.sleep(1000)
        }
      }
      localAfterFun()
      threadIsActive = false
      println("Thread finished normal")
    }
  }

  def chooseFile():String = {
    val chooser = new JFileChooser(".")
    chooser.showOpenDialog(new JFrame())
    chooser.setDialogTitle("Choose file:")
    val file = chooser.getSelectedFile
    if(file!=null){
      return file.getAbsolutePath
    }
    ""
  }

  def saveFile():String = {
    val chooser = new JFileChooser(".")
    chooser.showSaveDialog(new JFrame())
    chooser.setDialogTitle("Save file:")
    val file = chooser.getSelectedFile
    if(file!=null){
      return file.getAbsolutePath
    }
    ""
  }
}
