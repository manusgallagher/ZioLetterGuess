import zio._
import zio.clock.Clock
import zio.console.Console

import scala.util.Random

object Runner {

  def getInput(allowedChars: List[Char]): ZIO[console.Console, String, Char] = for {
    _           <- console.putStrLn("\nGuess a letter:")
    input       <- console.getStrLn.orElseFail("`console.getStrLn` failed.")
    sanitised   <- UIO(input.replace(" ", ""))
    validLength <- if (sanitised.length == 1) ZIO.succeed(sanitised.toLowerCase().head)
                   else ZIO.fail("Invalid length, Input length must equal 1.")
    validChar   <- if (allowedChars.contains(validLength)) ZIO.succeed(validLength)
                   else ZIO.fail("Not a valid Input, Input must be a letter.")
  } yield validChar

  def game(chosenLetter: Char, allowedChars: List[Char]): ZIO[Console with Clock, String, String] = for {
    input <- getInput(allowedChars).tapError(err => console.putStrLn(err)).retry(Schedule.forever)
    res   <- if (input == chosenLetter) ZIO.succeed(s"Congrats, $input is a match!")
             else ZIO.fail(s"$input is not a match!\n")
  } yield res

  def randomLetter(c: List[Char]): UIO[Char] = UIO(c(Random.nextInt(c.size)))

  def chooseLevel: ZIO[Console with Clock, String, Schedule[Any, Any, Long]] = for {
    _         <- console.putStrLn(s"Choose one of the following levels: ${Levels.levelsMap.keys.mkString(", ")}")
    userInput <- console.getStrLn.orElseFail("`console.getStrLn` failed.")
    schedule  <- ZIO
                   .fromOption(Levels.apply(userInput))
                   .foldM(
                     _ => console.putStrLn("Not a valid Level, try again!\n") *> chooseLevel,
                     s => UIO(s.retries)
                   )
  } yield schedule

  def main(args: Array[String]): Unit = {
    val runtime = zio.Runtime.default
    val effect  = for {
      _            <- console.putStrLn("Welcome to the Letter Guessing game!\n")
      allowedChars <- UIO(('a' to 'z').toList)
      chosenLetter <- randomLetter(allowedChars)
      level        <- chooseLevel
      res          <- game(chosenLetter, allowedChars)
                        .tapBoth(err => console.putStrLn(err), succ => console.putStrLn(succ))
                        .retry(level)
                        .tapError(_ => console.putStrLn("Attempts Exceeded, GAME OVER!"))
                        .orElseSucceed()
    } yield res

    runtime.unsafeRun(effect)
  }
}
