import java.io.File

import scala.collection.mutable
import scala.io.Source

object main extends App {

  val MIL_SECONDS_BETWEEN_AUTO_MOVES = 3000

  def titlePrint(): Unit = {
    // TODO add clear of console
    println("==========================")
    println("Bloxorz game")
    println("==========================")
  }

  def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    d.listFiles.filter(_.getName.endsWith("txt")).toList
  }

  val gameLogic = new GameLogic()
  val mapEditing = new MapEditing()
  val moveSet = gameLogic.move(2)(_, _, _, _, _)

  var isExit = false
  while(!isExit){
    titlePrint()
    println("Menu:")
    println("1. Play game")
    println("2. Calculate win moves sequence")
    println("3. Create new map")
    println("4. Exit")
    println("Choose option:")
    val menuOption = scala.io.StdIn.readLine()
    if (menuOption.charAt(0) == '1'){
      var isMapOk = false
      var mapName = ""
        while(!isMapOk) {
          titlePrint()
          println("Available maps:")
          val maps = getListOfFiles("./data/maps")
          var cnt = 0
          for (map <- maps) {
            cnt += 1
            println(cnt + ": " + map.getName)
          }
          println("Choose map:")
          val inputNum = scala.io.StdIn.readLine()
          try {
            val mapNum = Integer.parseInt(inputNum)
            mapName = maps(mapNum-1).getAbsolutePath
            isMapOk = true
          }
          catch {
            case _: Throwable =>
          }
        }
      val curr_board = gameLogic.loadBoardFromFile(mapName)
      var position = gameLogic.findStartPosition(curr_board)
      gameLogic.movementWriter(true, position._1, position._2, position._3, position._4, curr_board)
      titlePrint()
      println(gameLogic.boardData(curr_board))
      var state = 3
      val moveBuffer = mutable.ListBuffer[String]()
      while (state == 3){
        var temp = ""
        if (moveBuffer.isEmpty){
          println("Choose movement (u-up, d-down, l-left, r-right, f-load from file, q-quit):")
          temp = scala.io.StdIn.readLine()
        }
        else{
          temp = moveBuffer.remove(0)
          println("Playing movement: " + temp)
          Thread.sleep(MIL_SECONDS_BETWEEN_AUTO_MOVES)
        }
        if(temp == "d" || temp == "u" || temp == "l" || temp == "r") {
          gameLogic.movementWriter(false, position._1, position._2, position._3, position._4, curr_board)
          position = moveSet(temp.charAt(0), position._1, position._2, position._3, position._4)
          gameLogic.movementWriter(true, position._1, position._2, position._3, position._4, curr_board)
          state = gameLogic.afterMoveLogic(curr_board, position._1, position._2, position._3, position._4)
        }
        if(temp == "f"){
          var isMoveOk = false
          var moveData = ""
          while(!isMoveOk) {
            titlePrint()
            println("Available move files:")
            val moves = getListOfFiles("./data/moves")
            var cnt = 0
            for (move <- moves) {
              cnt += 1
              println(cnt + ": " + move.getName)
            }
            println("Choose move file:")
            val inputNum = scala.io.StdIn.readLine()
            try {
              val mapNum = Integer.parseInt(inputNum)
              moveData = moves(mapNum-1).getAbsolutePath
              isMoveOk = true
            }
            catch {
              case _: Throwable =>
            }
          }
          val dataStr = Source.fromFile(moveData).mkString
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
        }
        if(temp == "q"){
          state = 5
        }
        else {
          titlePrint()
          println(gameLogic.boardData(curr_board))
        }
      }
      if(state == 1){
        titlePrint()
        println("Win, congratulation!")
        println("Press enter to continue...")
        scala.io.StdIn.readLine()
      }
      else{
        if(state == 2) {
          titlePrint()
          println("You lose!")
          println("Press enter to continue...")
          scala.io.StdIn.readLine()
        }
      }
    }
    else{
      if(menuOption.charAt(0) == '2'){
        var isMapOk = false
        var mapName = ""
        while(!isMapOk) {
          titlePrint()
          println("Available maps:")
          val maps = getListOfFiles("./data/maps")
          var cnt = 0
          for (map <- maps) {
            cnt += 1
            println(cnt + ": " + map.getName)
          }
          println("Choose map:")
          val inputNum = scala.io.StdIn.readLine()
          try {
            val mapNum = Integer.parseInt(inputNum)
            mapName = maps(mapNum-1).getAbsolutePath
            isMapOk = true
          }
          catch {
            case _: Throwable =>
          }
        }
        titlePrint()
        println("Choose name of file for moves (without extension):")
        val nameFile = scala.io.StdIn.readLine()
        val test_moves_board = gameLogic.loadBoardFromFile(mapName)
        titlePrint()
        println("Calculating moves...")
        val retMoves = gameLogic.calculateWinMove(test_moves_board)
        if (retMoves != "") {
          gameLogic.saveMovesToFile(nameFile, retMoves)
          titlePrint()
          println("Moves calculated: " + retMoves)
          println("Press enter to continue...")
        }
        else{
          titlePrint()
          println("There is no win sequence.")
          println("Press enter to continue...")
        }
        scala.io.StdIn.readLine()
      }
      else {
        if(menuOption.charAt(0) == '3'){
          var isMapOk = false
          var mapName = ""
          while(!isMapOk) {
            titlePrint()
            println("Available maps:")
            val maps = getListOfFiles("./data/maps")
            var cnt = 0
            for (map <- maps) {
              cnt += 1
              println(cnt + ": " + map.getName)
            }
            println("Choose map:")
            val inputNum = scala.io.StdIn.readLine()
            try {
              val mapNum = Integer.parseInt(inputNum)
              mapName = maps(mapNum-1).getAbsolutePath
              isMapOk = true
            }
            catch {
              case _: Throwable =>
            }
          }
          val curr_board = gameLogic.loadBoardFromFile(mapName)
          var isFinish = false
          var position = gameLogic.findStartPosition(curr_board)
          gameLogic.movementWriter(true, position._1, position._2, position._3, position._4, curr_board)
          while(!isFinish){
            val isOnEdge = mapEditing.isBlockableEdge(position._1, position._3, curr_board)
            val isBlockEdge = mapEditing.isBlockOnEdge(position._1, position._3, curr_board)
            titlePrint()
            println(gameLogic.boardData(curr_board))
            println("Map editing:")
            println("l, r, u, d: Move edit pointer (l-left, d-down, r-right, u-up)")
            if(isOnEdge) {
              println("ab: Add block on edge of terrain (position of edit pointer)")
            }
            if(isBlockEdge){
              println("rb: Remove block on edge of terrain (position of edit pointer)")
            }
            println("2. Create new composite operation")
            println("3. Cancel")
            val temp = scala.io.StdIn.readLine()
            if(temp == "l" || temp == "r" || temp == "u" || temp == "d"){
              gameLogic.movementWriter(false, position._1, position._2, position._3, position._4, curr_board)
              position = mapEditing.move(temp.charAt(0), position._1, position._3, curr_board)
              gameLogic.movementWriter(true, position._1, position._2, position._3, position._4, curr_board)
            }
            else{
              if(temp == "ab" && isOnEdge){
                mapEditing.addBlock(position._1, position._3, curr_board)
              }
              else{
                if(temp == "rb" && isBlockEdge){
                  mapEditing.removeBlock(position._1, position._3, curr_board)
                }
                else{
                  isFinish = true
                }
              }
            }
          }

        }
        else{
          if(menuOption.charAt(0) == '4'){
            isExit = true
          }
        }
      }
    }
  }


}
