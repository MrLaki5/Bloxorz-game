import board.Field
import gameTools.GameLogic
import javax.swing.{JFileChooser, JFrame}
import mapTools.{MapEditing, SequenceOp}
import scala.collection.mutable
import scala.io.Source
import scala.swing.event.ButtonClicked
import scala.swing.{BorderPanel, Button, Component, Dimension, GridPanel, Label, MainFrame, SimpleSwingApplication, TextField}


//Game GUI part
object mainGUI extends SimpleSwingApplication {

  type BoardType = mutable.ListBuffer[mutable.ListBuffer[Field]]
  var board: BoardType = _
  var position: (Int, Int, Int, Int) = _
  val moveBuffer = mutable.ListBuffer[String]()
  val allOperations = mutable.ListBuffer[String]()
  var chosenOperations = mutable.ListBuffer[String]()
  val sequenceOperations = mutable.ListBuffer[SequenceOp]()
  var argumentsOperations = mutable.ListBuffer[Int]()
  val allOperationsComposite = mutable.ListBuffer[String]()
  val compositeOperations = mutable.ListBuffer[(String, (Int, Int, BoardType) => (Int, Int, Int, Int))]()
  var isComposite = true
  val moveRun = new moveRunnable()
  var moveThread: Thread = null
  var threadIsActive = false
  val threadSemaphore = "threadSem"

  // Used for window setup
  def top= new MainFrame {
    // Window attributes
    title = "Bloxorz-game"
    preferredSize = new Dimension(640, 480)
    resizable = false
    peer.setSize(new Dimension(640, 480))
    peer.setLocationRelativeTo(null)
    // Main menu items
    val mainMenuStartGameButton = new Button {
      text = "Play"
      borderPainted = true
    }
    val mainMenuCalculateButton = new Button {
      text = "Calculate win sequence"
      borderPainted = true
    }
    val mainMenuCreateMapButton = new Button {
      text = "Create map"
      borderPainted = true
    }
    val mainMenuExitButton = new Button {
      text = "Exit"
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
      borderPainted = true
    }
    val gameRightButton = new Button {
      text = "Right"
      borderPainted = true
    }
    val gameUpButton = new Button {
      text = "Up"
      borderPainted = true
    }
    val gameDownButton = new Button {
      text = "Down"
      borderPainted = true
    }
    val gameFileButton = new Button {
      text = "File"
      borderPainted = true
    }
    val gameQuitButton = new Button {
      text = "Quit"
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
      borderPainted = true
    }
    val moveSequenceBackButton = new Button {
      text = "Back"
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
    // Create map items
    val newMapLeftButton = new Button {
      text = "Left"
      borderPainted = true
    }
    val newMapRightButton = new Button {
      text = "Right"
      borderPainted = true
    }
    val newMapUpButton = new Button {
      text = "Up"
      borderPainted = true
    }
    val newMapDownButton = new Button {
      text = "Down"
      borderPainted = true
    }
    val newMapAddBlockButton = new Button {
      text = "Add block"
      borderPainted = true
    }
    val newMapRemoveBlockButton = new Button {
      text = "Rm block"
      borderPainted = true
    }
    val newMapAddSpecialButton = new Button {
      text = "Add special"
      borderPainted = true
    }
    val newMapRemoveSpecialButton = new Button {
      text = "Rm special"
      borderPainted = true
    }
    val newMapPutStartButton = new Button {
      text = "Put start"
      borderPainted = true
    }
    val newMapPutFinishButton = new Button {
      text = "Put finish"
      borderPainted = true
    }
    val newMapInvertButton = new Button {
      text = "Invert"
      borderPainted = true
    }
    val newMapRemoveAllSpecialButton = new Button {
      text = "Rm all special"
      borderPainted = true
    }
    val newMapRemoveNSpecialButton = new Button {
      text = "Filter"
      borderPainted = true
    }
    val newMapCreateSequenceButton = new Button {
      text = "Mk sequence"
      borderPainted = true
    }
    val newMapUseSequenceButton = new Button {
      text = "Use sequence"
      borderPainted = true
    }
    val newMapCreateCompositeButton = new Button {
      text = "Mk composite"
      borderPainted = true
    }
    val newMapUseCompositeButton = new Button {
      text = "Use composite"
      borderPainted = true
    }
    val newMapSaveButton = new Button {
      text = "Save"
      borderPainted = true
    }
    val newMapCancelButton = new Button {
      text = "Cancel"
      borderPainted = true
    }
    val newMapButtonsGrid =  new GridPanel(3, 6) {
      contents += newMapLeftButton
      contents += newMapRightButton
      contents += newMapUpButton
      contents += newMapDownButton
      contents += newMapAddBlockButton
      contents += newMapRemoveBlockButton
      contents += newMapAddSpecialButton
      contents += newMapRemoveSpecialButton
      contents += newMapPutStartButton
      contents += newMapPutFinishButton
      contents += newMapInvertButton
      contents += newMapRemoveAllSpecialButton
      contents += newMapRemoveNSpecialButton
      contents += newMapCreateSequenceButton
      contents += newMapUseSequenceButton
      contents += newMapCreateCompositeButton
      contents += newMapUseCompositeButton
      contents += newMapSaveButton
      contents += newMapCancelButton
    }
    listenTo(newMapLeftButton)
    listenTo(newMapRightButton)
    listenTo(newMapUpButton)
    listenTo(newMapDownButton)
    listenTo(newMapAddBlockButton)
    listenTo(newMapRemoveBlockButton)
    listenTo(newMapAddSpecialButton)
    listenTo(newMapRemoveSpecialButton)
    listenTo(newMapPutStartButton)
    listenTo(newMapPutFinishButton)
    listenTo(newMapCancelButton)
    listenTo(newMapInvertButton)
    listenTo(newMapRemoveNSpecialButton)
    listenTo(newMapRemoveAllSpecialButton)
    listenTo(newMapSaveButton)
    listenTo(newMapCreateSequenceButton)
    listenTo(newMapUseSequenceButton)
    listenTo(newMapCreateCompositeButton)
    listenTo(newMapUseCompositeButton)
    // Remove N special menu
    val newMapRmNRemoveButton = new Button {
      text = "Remove"
      borderPainted = true
    }
    val newMapRmNCancelButton = new Button {
      text = "Cancel"
      borderPainted = true
    }
    val newMapRmNNumber = new TextField{}
    val newMapRmNGrid =  new GridPanel(2, 2) {
      contents += new Label("Enter number N: ")
      contents += newMapRmNNumber
      contents += newMapRmNRemoveButton
      contents += newMapRmNCancelButton
    }
    listenTo(newMapRmNRemoveButton)
    listenTo(newMapRmNCancelButton)
    // Create operation sequence items
    allOperations += "move left"
    allOperations += "move right"
    allOperations += "move down"
    allOperations += "move up"
    allOperations += "add block"
    allOperations += "rm block"
    allOperations += "add special"
    allOperations += "rm special"
    allOperations += "put finish"
    allOperations += "put start"
    allOperations += "invert"
    allOperations += "rm all special"
    allOperations += "filter"
    allOperationsComposite += "move left"
    allOperationsComposite += "move right"
    allOperationsComposite += "move down"
    allOperationsComposite += "move up"
    allOperationsComposite += "add block"
    allOperationsComposite += "rm block"
    allOperationsComposite += "add special"
    allOperationsComposite += "rm special"
    allOperationsComposite += "put finish"
    allOperationsComposite += "put start"
    allOperationsComposite += "invert"
    allOperationsComposite += "rm all special"
    allOperationsComposite += "filter"
    var opSeqAllPossibleItemsList = new swing.ListView[String](allOperations)
    var opSeqChosenItemsList = new swing.ListView[String](chosenOperations)
    val opSeqOperationName = new TextField()
    val opSeqOperationArg = new TextField()
    val opSeqAddOpButton = new Button {
      text = ">>"
      borderPainted = true
    }
    val opSeqRemoveOpButton = new Button {
      text = "<<"
      borderPainted = true
    }
    val opSeqSaveOpButton = new Button {
      text = "Save"
      borderPainted = true
    }
    val opSeqCancelButton = new Button {
      text = "Cancel"
      borderPainted = true
    }
    val opSeqButtonsGrid = new GridPanel(8, 1){
      contents += new Label("Operation name:")
      contents += opSeqOperationName
      contents += new Label("Operation argument:")
      contents += opSeqOperationArg
      contents += opSeqAddOpButton
      contents += opSeqRemoveOpButton
      contents += opSeqSaveOpButton
      contents += opSeqCancelButton
    }
    var opSeqGrid =  new GridPanel(1, 3) {
      contents += opSeqAllPossibleItemsList
      contents += opSeqButtonsGrid
      contents += opSeqChosenItemsList
    }
    listenTo(opSeqAddOpButton)
    listenTo(opSeqRemoveOpButton)
    listenTo(opSeqSaveOpButton)
    listenTo(opSeqCancelButton)
    // Use sequence operation
    var useSeqCommandsList = new swing.ListView[String](chosenOperations)
    val useSeqCancelButton = new Button {
      text = "Cancel"
      borderPainted = true
    }
    val useSeqActivateButton = new Button {
      text = "Use command"
      borderPainted = true
    }
    var useSeqGridButton =  new GridPanel(1, 2) {
      contents += useSeqCancelButton
      contents += useSeqActivateButton
    }
    listenTo(useSeqActivateButton)
    listenTo(useSeqCancelButton)
    // Set window context to main menu
    contents = mainMenuGrid
    // react to events
    reactions += {
      case ButtonClicked(component: Component) if component == mainMenuExitButton =>
        quit()
      case ButtonClicked(component) if component == mainMenuStartGameButton =>
        val fileName = chooseFile()
        if(fileName != ""){
          try{
            board = GameLogic.loadBoardFromFile(fileName)
            position = GameLogic.findStartPosition(board)
            GameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
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
        GameLogic.movementWriter(false, position._1, position._2, position._3, position._4, board)
        position = GameLogic.move(2)(moveCh, position._1, position._2, position._3, position._4)
        val isMoveOk = GameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
        if(!isMoveOk){
          labelFin.text = "You lose!"
          contents = new BorderPanel {
            layout(canvas) = BorderPanel.Position.Center
            layout(gameFinishGrid) = BorderPanel.Position.South
          }
        }
        canvas.setBoard(board)
        val state = GameLogic.afterMoveLogic(board, position._1, position._2, position._3, position._4)
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
      case ButtonClicked(component) if component == gameFinishButton || component == moveSequenceBackButton || component == newMapCancelButton=>
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
            board = GameLogic.loadBoardFromFile(fileName)
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
          val retMoves = GameLogic.calculateWinMove(board)
          moveSequenceGenerateButton.enabled = false
          if (retMoves != "") {
            GameLogic.saveMovesToFile(fileName, retMoves)
            moveSequenceStatus.text = "Moves calculated: " + retMoves
          }
          else{
            moveSequenceStatus.text = "There is no win sequence."
          }
        }
      case ButtonClicked(comment) if comment == mainMenuCreateMapButton =>
        val fileName = chooseFile()
        if(fileName != ""){
          try{
            board = GameLogic.loadBoardFromFile(fileName)
            position = GameLogic.findStartPosition(board)
            GameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
            mapEditActivateButtons()
            contents = new BorderPanel {
              layout(canvas) = BorderPanel.Position.Center
              layout(newMapButtonsGrid) = BorderPanel.Position.South
            }
            canvas.setBoard(board)
          }
          catch {
            case e: Throwable => println(e)
          }
        }
      case ButtonClicked(component) if component == newMapLeftButton || component == newMapRightButton || component == newMapDownButton || component == newMapUpButton =>
        var moveCh = 'a'
        if (component==newMapLeftButton){
          moveCh = 'l'
        }
        else{
          if (component==newMapRightButton){
            moveCh = 'r'
          }
          else{
            if(component==newMapDownButton){
              moveCh = 'd'
            }
            else{
              moveCh = 'u'
            }
          }
        }
        GameLogic.movementWriter(false, position._1, position._2, position._3, position._4, board)
        position = MapEditing.move(None)(moveCh)(position._1, position._3, board)
        GameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
        canvas.setBoard(board)
        mapEditActivateButtons()
      case ButtonClicked(component) if component == newMapAddBlockButton =>
        MapEditing.addBlock(None)(position._1, position._3, board)
        canvas.setBoard(board)
        mapEditActivateButtons()
      case ButtonClicked(component) if component == newMapRemoveBlockButton =>
        MapEditing.removeBlock(None)(position._1, position._3, board)
        canvas.setBoard(board)
        mapEditActivateButtons()
      case ButtonClicked(component) if component == newMapAddSpecialButton =>
        MapEditing.addSpecial(None)(position._1, position._3, board)
        canvas.setBoard(board)
        mapEditActivateButtons()
      case ButtonClicked(component) if component == newMapRemoveSpecialButton =>
        MapEditing.removeSpecial(None)(position._1, position._3, board)
        canvas.setBoard(board)
        mapEditActivateButtons()
      case ButtonClicked(component) if component == newMapPutStartButton =>
        MapEditing.changeStart(None)(position._1, position._3, board)
        canvas.setBoard(board)
        mapEditActivateButtons()
      case ButtonClicked(component) if component == newMapPutFinishButton =>
        MapEditing.changeFinish(None)(position._1, position._3, board)
        canvas.setBoard(board)
        mapEditActivateButtons()
      case ButtonClicked(component) if component == newMapInvertButton =>
        MapEditing.inversion(None)(position._1, position._3, board)
        GameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
        canvas.setBoard(board)
        mapEditActivateButtons()
      case ButtonClicked(component) if component == newMapRemoveAllSpecialButton =>
        MapEditing.removeAllSpecial(None)(position._1, position._3, board)
        GameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
        canvas.setBoard(board)
        mapEditActivateButtons()
      case ButtonClicked(component) if component == newMapRemoveNSpecialButton =>
        newMapRmNNumber.text = ""
        contents = new BorderPanel {
          layout(canvas) = BorderPanel.Position.Center
          layout(newMapRmNGrid) = BorderPanel.Position.South
        }
      case ButtonClicked(component) if component == newMapRmNCancelButton || component == useSeqCancelButton =>
        contents = new BorderPanel {
          layout(canvas) = BorderPanel.Position.Center
          layout(newMapButtonsGrid) = BorderPanel.Position.South
        }
      case ButtonClicked(component) if component == newMapRmNRemoveButton =>
        try{
          val num = Integer.parseInt(newMapRmNNumber.text)
          MapEditing.filter(None)(num)(position._1, position._3, board)
          contents = new BorderPanel {
            layout(canvas) = BorderPanel.Position.Center
            layout(newMapButtonsGrid) = BorderPanel.Position.South
          }
          GameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
          mapEditActivateButtons()
          canvas.setBoard(board)
        }
        catch{
          case _:Throwable =>
        }
      case ButtonClicked(component) if component == newMapSaveButton =>
        val fileName = saveFile()
        if(fileName != ""){
          GameLogic.movementWriter(false, position._1, position._2, position._3, position._4, board)
          GameLogic.saveBoardToFile(fileName, board)
          contents = mainMenuGrid
          mainMenuGrid.repaint()
        }
      case ButtonClicked(component) if component == newMapCreateSequenceButton =>
        chosenOperations.clear()
        argumentsOperations.clear()
        isComposite = false
        opSeqAllPossibleItemsList = new swing.ListView[String](allOperations)
        opSeqChosenItemsList = new swing.ListView[String](chosenOperations)
        opSeqOperationName.text = ""
        opSeqOperationArg.text = ""
        opSeqGrid =  new GridPanel(1, 3) {
          contents += opSeqAllPossibleItemsList
          contents += opSeqButtonsGrid
          contents += opSeqChosenItemsList
        }
        contents = opSeqGrid
        opSeqGrid.repaint()
      case ButtonClicked(component) if component == newMapCreateCompositeButton =>
        chosenOperations.clear()
        argumentsOperations.clear()
        isComposite = true
        opSeqAllPossibleItemsList = new swing.ListView[String](allOperationsComposite)
        opSeqChosenItemsList = new swing.ListView[String](chosenOperations)
        opSeqOperationName.text = ""
        opSeqOperationArg.text = ""
        opSeqGrid =  new GridPanel(1, 3) {
          contents += opSeqAllPossibleItemsList
          contents += opSeqButtonsGrid
          contents += opSeqChosenItemsList
        }
        contents = opSeqGrid
        opSeqGrid.repaint()
      case ButtonClicked(component) if component == opSeqCancelButton =>
        contents = new BorderPanel {
          layout(canvas) = BorderPanel.Position.Center
          layout(newMapButtonsGrid) = BorderPanel.Position.South
        }
      case ButtonClicked(component) if component == opSeqAddOpButton =>
        val useAllOperations = if(isComposite) allOperationsComposite else allOperations
        if(opSeqAllPossibleItemsList.selection.anchorIndex >= 0 && opSeqAllPossibleItemsList.selection.anchorIndex < useAllOperations.size) {
          val currOperation = useAllOperations(opSeqAllPossibleItemsList.selection.anchorIndex)
          if(currOperation == "filter"){
            try{
              argumentsOperations += Integer.parseInt(opSeqOperationArg.text)
              chosenOperations += currOperation
              opSeqChosenItemsList = new swing.ListView[String](chosenOperations)
              opSeqGrid = new GridPanel(1, 3) {
                contents += opSeqAllPossibleItemsList
                contents += opSeqButtonsGrid
                contents += opSeqChosenItemsList
              }
              contents = opSeqGrid
              opSeqGrid.repaint()
            }
            catch {
              case _: Throwable =>
            }
          }
          else {
            chosenOperations += currOperation
            opSeqChosenItemsList = new swing.ListView[String](chosenOperations)
            opSeqGrid = new GridPanel(1, 3) {
              contents += opSeqAllPossibleItemsList
              contents += opSeqButtonsGrid
              contents += opSeqChosenItemsList
            }
            contents = opSeqGrid
            opSeqGrid.repaint()
          }
        }
      case ButtonClicked(component) if component == opSeqRemoveOpButton =>
        if(opSeqChosenItemsList.selection.anchorIndex >= 0 && opSeqChosenItemsList.selection.anchorIndex < chosenOperations.size){
          val rmNum = opSeqChosenItemsList.selection.anchorIndex
          val rmOperation = chosenOperations.remove(rmNum)
          if (rmOperation == "filter"){
            var numArg = 0
            for(i <- Range(0, rmNum)){
              if(chosenOperations(i) == "filter"){
                numArg += 1
              }
            }
            argumentsOperations.remove(numArg)
          }
          opSeqChosenItemsList = new swing.ListView[String](chosenOperations)
          opSeqGrid =  new GridPanel(1, 3) {
            contents += opSeqAllPossibleItemsList
            contents += opSeqButtonsGrid
            contents += opSeqChosenItemsList
          }
          contents = opSeqGrid
          opSeqGrid.repaint()
        }
      case ButtonClicked(component) if component == opSeqSaveOpButton =>
        if(!opSeqOperationName.text.isEmpty && chosenOperations.size > 1){
          if(!checkNameExists(opSeqOperationName.text)){
            if(!isComposite){
              val chosenOperationFunctions = new mutable.ListBuffer[(Int, Int, BoardType) => (Int, Int, Int, Int)]()
              var argumentNum = 0
              for(operation <- chosenOperations){
                operation match {
                  case "move left" =>
                    chosenOperationFunctions += MapEditing.move(None)('l')
                  case "move right" =>
                    chosenOperationFunctions += MapEditing.move(None)('r')
                  case "move up" =>
                    chosenOperationFunctions += MapEditing.move(None)('u')
                  case "move down" =>
                    chosenOperationFunctions += MapEditing.move(None)('d')
                  case "add block" =>
                    chosenOperationFunctions += MapEditing.addBlock(None)
                  case "rm block" =>
                    chosenOperationFunctions += MapEditing.removeBlock(None)
                  case "add special" =>
                    chosenOperationFunctions += MapEditing.addSpecial(None)
                  case "rm special" =>
                    chosenOperationFunctions += MapEditing.addSpecial(None)
                  case "put start" =>
                    chosenOperationFunctions += MapEditing.changeStart(None)
                  case "put finish" =>
                    chosenOperationFunctions += MapEditing.changeFinish(None)
                  case "invert" =>
                    chosenOperationFunctions += MapEditing.inversion(None)
                  case "rm all special" =>
                    chosenOperationFunctions += MapEditing.removeAllSpecial(None)
                  case "filter" =>
                    val curr_arg = argumentsOperations(argumentNum)
                    chosenOperationFunctions += MapEditing.filter(None)(curr_arg)
                    argumentNum += 1
                  case _ =>
                    for(sOp <- sequenceOperations){
                      if(sOp.checkName(operation)){
                        chosenOperationFunctions += sOp.doOperation(sequenceOperations)
                      }
                    }
                }
              }
              val sOpj = new SequenceOp(opSeqOperationName.text, chosenOperationFunctions)
              chosenOperations = new mutable.ListBuffer[String]()
              argumentsOperations = new mutable.ListBuffer[Int]()
              sequenceOperations += sOpj
              allOperations += opSeqOperationName.text
              contents = new BorderPanel {
                layout(canvas) = BorderPanel.Position.Center
                layout(newMapButtonsGrid) = BorderPanel.Position.South
              }
            }
            else{
              chosenOperations = chosenOperations.reverse
              argumentsOperations = argumentsOperations.reverse
              var buildFunction: Option[(Int, Int, BoardType) => (Int, Int, Int, Int)] = None
              var argCnt = 0
              for(operation <- chosenOperations){
                val cur_operation = buildFunction
                operation match {
                  case "move left" =>
                    buildFunction = Some(MapEditing.move(cur_operation)('l')(_, _, _))
                  case "move right" =>
                    buildFunction = Some(MapEditing.move(cur_operation)('r')(_, _, _))
                  case "move up" =>
                    buildFunction = Some(MapEditing.move(cur_operation)('u')(_, _, _))
                  case "move down" =>
                    buildFunction = Some(MapEditing.move(cur_operation)('d')(_, _, _))
                  case "add block" =>
                    buildFunction = Some(MapEditing.addBlock(cur_operation)(_, _, _))
                  case "rm block" =>
                    buildFunction = Some(MapEditing.removeBlock(cur_operation)(_, _, _))
                  case "add special" =>
                    buildFunction = Some(MapEditing.addSpecial(cur_operation)(_, _, _))
                  case "rm special" =>
                    buildFunction = Some(MapEditing.removeSpecial(cur_operation)(_, _, _))
                  case "put start" =>
                    buildFunction = Some(MapEditing.changeStart(cur_operation)(_, _, _))
                  case "put finish" =>
                    buildFunction = Some(MapEditing.changeFinish(cur_operation)(_, _, _))
                  case "invert" =>
                    buildFunction = Some(MapEditing.inversion(cur_operation)(_, _, _))
                  case "rm all special" =>
                    buildFunction = Some(MapEditing.removeAllSpecial(cur_operation)(_, _, _))
                  case "filter" =>
                    val currArgCnt = argCnt
                    val argumentValue = argumentsOperations(currArgCnt)
                    buildFunction = Some(MapEditing.filter(cur_operation)(argumentValue)(_, _, _))
                    argCnt += 1
                  case _ =>
                    for(i <- compositeOperations){
                      if(i._1 == operation){
                        val tempF = (x: Int, y: Int, board1: BoardType) => {val pos = i._2(x, y, board1); cur_operation.get.apply(pos._1, pos._3, board1) }
                        buildFunction = Some(tempF)
                      }
                    }
                }
              }
              chosenOperations = new mutable.ListBuffer[String]()
              argumentsOperations = new mutable.ListBuffer[Int]()
              val comElem = (opSeqOperationName.text, buildFunction.get)
              compositeOperations += comElem
              allOperationsComposite += opSeqOperationName.text
              contents = new BorderPanel {
                layout(canvas) = BorderPanel.Position.Center
                layout(newMapButtonsGrid) = BorderPanel.Position.South
              }
            }
          }
        }
      case ButtonClicked(component) if component == newMapUseSequenceButton =>
        val opNames = new mutable.ListBuffer[String]()
        for(i<-sequenceOperations){
          opNames += i.operationName
        }
        isComposite = false
        useSeqCommandsList = new swing.ListView[String](opNames)
        contents = new BorderPanel {
          layout(useSeqCommandsList) = BorderPanel.Position.Center
          layout(useSeqGridButton) = BorderPanel.Position.South
        }
      case ButtonClicked(component) if component == newMapUseCompositeButton =>
        val opNames = new mutable.ListBuffer[String]()
        for(i<-compositeOperations){
          opNames += i._1
        }
        isComposite = true
        useSeqCommandsList = new swing.ListView[String](opNames)
        contents = new BorderPanel {
          layout(useSeqCommandsList) = BorderPanel.Position.Center
          layout(useSeqGridButton) = BorderPanel.Position.South
        }
      case ButtonClicked(component) if component == useSeqActivateButton =>
        if(!isComposite){
          if(useSeqCommandsList.selection.anchorIndex >= 0 && useSeqCommandsList.selection.anchorIndex < sequenceOperations.size){
            position = sequenceOperations(useSeqCommandsList.selection.anchorIndex).doOperation(sequenceOperations)(position._1, position._3, board)
            MapEditing.removeCursor(board)
            GameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
            contents = new BorderPanel {
              layout(canvas) = BorderPanel.Position.Center
              layout(newMapButtonsGrid) = BorderPanel.Position.South
            }
            mapEditActivateButtons()
            canvas.setBoard(board)
          }
        }
        else{
          if(useSeqCommandsList.selection.anchorIndex >= 0 && useSeqCommandsList.selection.anchorIndex < compositeOperations.size){
            position = compositeOperations(useSeqCommandsList.selection.anchorIndex)._2.apply(position._1, position._3, board)
            MapEditing.removeCursor(board)
            GameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
            contents = new BorderPanel {
              layout(canvas) = BorderPanel.Position.Center
              layout(newMapButtonsGrid) = BorderPanel.Position.South
            }
            mapEditActivateButtons()
            canvas.setBoard(board)
          }
        }
    }

    // Set GUI if game is over or if player has won
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

    // Activate buttons after move sequence has been played
    def activateButtons(): Unit = {
      gameLeftButton.enabled = true
      gameRightButton.enabled = true
      gameUpButton.enabled = true
      gameDownButton.enabled = true
      gameFileButton.enabled = true
    }

    // Close thread that is playing move sequence
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

    // Change usability of map edit buttons depending on current position on map
    def mapEditActivateButtons(): Unit = {
      val isOnEdge = MapEditing.isBlockableEdge(position._1, position._3, board)
      val isBlockEdge = MapEditing.isBlockOnEdge(position._1, position._3, board)
      val isSpecial = board(position._1)(position._3).getSign() == '.'
      val isBlock = board(position._1)(position._3).getSign() == 'O'
      newMapAddBlockButton.enabled = isOnEdge
      newMapRemoveBlockButton.enabled = isBlockEdge
      newMapAddSpecialButton.enabled = isBlock
      newMapRemoveSpecialButton.enabled = isSpecial
      newMapPutStartButton.enabled = isBlock || isSpecial
      newMapPutFinishButton.enabled = isBlock || isSpecial
      newMapRemoveNSpecialButton.enabled = isSpecial
    }
  }


  // Thread used for playing win sequence
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
        GameLogic.movementWriter(false, position._1, position._2, position._3, position._4, board)
        position = GameLogic.move(2)(tempCommand.charAt(0), position._1, position._2, position._3, position._4)
        val isMoveOk = GameLogic.movementWriter(true, position._1, position._2, position._3, position._4, board)
        var state = 2
        if(isMoveOk){
          localCanvas.setBoard(board)
          state = GameLogic.afterMoveLogic(board, position._1, position._2, position._3, position._4)
        }
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

  // Function used for choosing file to open
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

  // Function used for choosing where to save file
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

  // Check if name of operation already exists
  def checkNameExists(wantedName: String):Boolean = {
    for(i<-allOperations){
      if(i == wantedName)
        return true
    }
    for(i<-allOperationsComposite){
      if(i == wantedName)
        return true
    }
    false
  }
}
