import scala.io.StdIn._
object Player {
  private val _board = Array.ofDim[Int](10, 10)
  def board: Array[Array[Int]] = _board

  def createShip(size: Int): Boolean = {
    print(s"Specify an alignment for your ship of size $size (v/h): ")
    val align = readChar()
    print(s"Specify a row for your ship of size $size (A-J): ")
    val rowSelect = readChar()
    val colSelect = readLine(s"Specify a column for your ship of size $size (1-10): ")
    val coordinates = List(rowSelect.toLower - 'a', colSelect.toInt - 1)
    if (coordinates.find(i => i < 0 || i > 9).isDefined) {
      println("Looks like you gave an invalid value in your coordinates. Try again!")
      Thread.sleep(1000)
      return false
    }

    align.toLower match {
      case 'v' =>
        val row = if (coordinates(0) + size > 10) 10 - size else coordinates(0)
        val col = coordinates(1)
        if (_board.slice(row, row + size).find(_(col) != 0).isDefined) {
          println("Looks like this ship would be placed on top of another ship. Try again!")
          Thread.sleep(1000)
          return false
        }
        (row until row + size).foreach { _board(_)(col) = size }

      case 'h' =>
        val row = coordinates(0)
        val col = if (coordinates(1) + size > 10) 10 - size else coordinates(1)
        if (_board(row).slice(col, col + size).exists(_ != 0)) {
          println("Looks like this ship would be placed on top of another ship. Try again!")
          Thread.sleep(1000)
          return false
        }
        (col until col + size).foreach { _board(row)(_) = size }

      case _ =>
        println("Looks like you gave an invalid value for alignment. Try again!")
        Thread.sleep(1000)
        return false
    }
    true
  }

  def initShips(x: List[Int] = List(2, 3, 3, 4, 5)): Unit = {
    println(this)
    if (x.length > 0) {
      if (createShip(x.head)) initShips(x.tail) else initShips(x)
    }
  }

  def makeHit(): List[Int] = {
    print("Specify a row to attack (A-J): ")
    val rowSelect = readChar()
    val colSelect = readLine(s"Specify a column to fire on (1-10): ")
    val coordinates = List(rowSelect.toLower - 'a', colSelect.toInt - 1)
    if (coordinates.find(i => i < 0 || i > 9).isDefined) {
      println("Looks like you gave an invalid value in your coordinates. Try again!")
      Thread.sleep(1000)
      return null
    }
    coordinates
  }

  def takeHit(hit: List[Int]): Boolean = {
    if (hit == null) false
    else if (_board(hit(0))(hit(1)) < 0) false
    else if (_board(hit(0))(hit(1)) != 0) {
      _board(hit(0))(hit(1)) = -2
      true
    } else {
      _board(hit(0))(hit(1)) = -1
      true
    }
  }

  def checkLoss(): Boolean = !_board.find(_.find(_ > 1).isDefined).isDefined

  override def toString: String = {
    val finalBoard = ('A' to 'J')
      .zip(_board.map(_.map(stringify)))
      .map { case (c, s) => (c +: s).mkString(" ") }
    "  1 2 3 4 5 6 7 8 9 10\n" + finalBoard.mkString("\n")
  }

  private val stringify = Map(
    -2 -> 'X',
    -1 -> '+',
    0 -> '-',
    2 -> 'D',
    3 -> 'C',
    4 -> 'B',
    5 -> 'A'
  )
}
