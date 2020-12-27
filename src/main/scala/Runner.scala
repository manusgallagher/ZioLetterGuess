import zio._
import zio.clock.Clock
import zio.console.Console

import scala.util.Random

object Runner {
  val allowedChars: List[Char] = ('a' to 'z').toList

  def getInput: ZIO[console.Console, String, Char] = for {
    _    <- console.putStrLn("Guess a letter:")
    in   <- console.getStrLn.orElseFail("Unable to get String")
    len  <- if (in.length == 1) ZIO.succeed(in.toLowerCase().head)
            else console.putStrLn("Input length must equal 1.") *> ZIO.fail("Invalid length")
    char <- if (allowedChars.contains(len)) ZIO.succeed(len)
            else console.putStrLn("Input must be a letter.") *> ZIO.fail("Not a valid input")
  } yield char

  def game(letter: Char): ZIO[Console with Clock, String, Unit] = for {
    input <- getInput.retry(Schedule.forever)
    res   <- if (input == letter) console.putStrLn("That's a match") *> ZIO.succeed()
             else console.putStrLn("Not a match!\n") *> game(letter)
  } yield res

  def randomLetter(c: List[Char]): UIO[Char] = UIO(c(Random.nextInt(c.size)))

  def main(args: Array[String]): Unit = {
    val runtime = zio.Runtime.default
    val effect  = for {
      _            <- console.putStrLn("Welcome to the Letter Guessing game! ")
      chosenLetter <- randomLetter(allowedChars)
      _            <- game(chosenLetter)
    } yield ()

    runtime.unsafeRun(effect)
  }
}
