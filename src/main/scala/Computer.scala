import scala.util.Random
object Computer {
  private val r = new Random()

  private val _board = Array.ofDim[Int](10, 10)
  def board: Array[Array[Int]] = _board

  private def createShip(size: Int): Boolean = {
    val coordinates = List(r.nextInt(10), r.nextInt(10))
    val align = if (r.nextInt(2) == 1) 'v' else 'h'

    align match {
      case 'v' =>
        val row = if (coordinates(0) + size > 10) 10 - size else coordinates(0)
        val col = coordinates(1)
        if (_board.slice(row, row + size).find(_(col) != 0).isDefined) return false
        (row until row + size).foreach { _board(_)(col) = size }

      case 'h' =>
        val row = coordinates(0)
        val col = if (coordinates(1) + size > 10) 10 - size else coordinates(1)
        if (_board(row).slice(col, col + size).exists(_ != 0)) return false
        (col until col + size).foreach { _board(row)(_) = size }

      case _ => return false
    }
    true
  }

  def initShips(): Unit = {
    if (List(2, 3, 3, 4, 5).map(createShip(_)).contains(false)) {
      _board.map(_.mapInPlace(i => i - i))
      initShips()
    }
  }

  def makeHit(): List[Int] = List(r.nextInt(10), r.nextInt(10))

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
      .map { case (c, s) => c + s.reduce(_ + _) }
    "  1 2 3 4 5 6 7 8 9 10\n" + finalBoard.reduce(_ + "\n" + _)
  }

  val stringify = Map(
    -2 -> " X",
    -1 -> " +",
    0 -> " -",
    2 -> " D",
    3 -> " C",
    4 -> " B",
    5 -> " A"
  )
}
