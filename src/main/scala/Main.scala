import scala.io.StdIn._
object Main extends App {
  Computer.initShips()

  println("Welcome to battleship!")
  Thread.sleep(500)
  println("Here is your board:")
  Thread.sleep(1000)
  println(Player)
  Player.initShips()

  var turn = 0
  var running = true
  while (turn < 200 && running) {
    (turn % 2) match {
      case 0 => // Player turn
        println("It's your turn! Here's the enemy's board:")
        if (turn <= 2) Thread.sleep(1500) else Thread.sleep(500)
        println(Computer)
        if (Computer.takeHit(Player.makeHit())) {
          println("Firing!")
          Thread.sleep(500)

          println(Computer)
          Thread.sleep(500)

          readLine("Press enter to continue.")

          turn += 1
          running = !Computer.checkLoss()
          if (!running) readLine("Hooray! You've won! Press enter to exit the program.")

        } else println("Sorry! That wasn't a valid move. Try again!"); Thread.sleep(1000)
      case 1 => // Computer turn
        if (Player.takeHit(Computer.makeHit())) {
          println("You've been attacked!")
          if (turn <= 3) Thread.sleep(1500) else Thread.sleep(500)

          println(Player)
          Thread.sleep(500)

          readLine("Press enter to continue.")

          turn += 1
          running = !Player.checkLoss()
          if (!running) readLine("Sorry, looks like you've lost... Press enter to exit the program.")
        }
    }
  }
}
