import zio.Schedule

trait Levels {
  val retries: Schedule[Any, Any, Long]
}

case object BEGINNER extends Levels {
  override val retries: Schedule[Any, Any, Long] = Schedule.forever
}

case object EASY extends Levels {
  override val retries: Schedule[Any, Any, Long] = Schedule.recurs(15)
}

case object MEDIUM extends Levels {
  override val retries: Schedule[Any, Any, Long] = Schedule.recurs(10)
}

case object HARD extends Levels {
  override val retries: Schedule[Any, Any, Long] = Schedule.recurs(5)
}

object Levels {
  def levelsMap: Map[String, Levels] = Map(
    "BEGINNER" -> BEGINNER,
    "EASY"     -> EASY,
    "MEDIUM"   -> MEDIUM,
    "HARD"     -> HARD
  )

  def apply(str: String): Option[Levels] = levelsMap.get(str.toUpperCase)
}
