import scala.io.StdIn._
object Main extends App {
  Computer.initShips()
  println(Computer)

  println("Welcome to battleship!")
  Thread.sleep(500)
  println("Here is your board:")
  Thread.sleep(1000)
  println(Player)

  var i = 0
  while (i < 5) {
    i match {
      case 0 => if (Player.createShip(2)) i += 1; println(Player)
      case 1 => if (Player.createShip(3)) i += 1; println(Player)
      case 2 => if (Player.createShip(3)) i += 1; println(Player)
      case 3 => if (Player.createShip(4)) i += 1; println(Player)
      case 4 => if (Player.createShip(5)) i += 1; println(Player)
    }
  }

  var turn = 0
  var running = true
  while (turn < 200 && running) {
    (turn % 2) match {
      case 0 => // Player turn
        println("It's your turn! Here's the enemy's board:")
        if (turn <= 4) Thread.sleep(1500) else Thread.sleep(500)
        println(Computer)
        if (Computer.takeHit(Player.makeHit())) {
          println("Firing!")
          Thread.sleep(500)

          println(Computer)
          Thread.sleep(500)

          print("Press enter to continue.")
          readLine()

          turn += 1
          running = !Computer.checkLoss()
          if (!running) {
            println("Hooray! You've won!")
            Thread.sleep(500)
            println("Press enter to exit the program.")
            readLine()
          }

        } else println("Sorry! That wasn't a valid move. Try again!"); Thread.sleep(1000)
      case 1 => // Computer turn
        if (Player.takeHit(Computer.makeHit())) {
          println("You've been attacked!")
          if (turn <= 5) Thread.sleep(1500) else Thread.sleep(500)

          println(Player)
          Thread.sleep(500)

          print("Press enter to continue.")
          readLine()

          turn += 1
          running = !Player.checkLoss()
          if (!running) {
            println("Sorry, looks like you've lost...")
            Thread.sleep(500)
            println("Press enter to exit the program.")
            readLine()
          }
        }
    }
  }
}
