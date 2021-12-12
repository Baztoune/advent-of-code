import scala.annotation.tailrec

val sample =
  """3,4,3,1,2""".stripMargin

val input =
  """2,1,2,1,5,1,5,1,2,2,1,1,5,1,4,4,4,3,1,2,2,3,4,1,1,5,1,1,4,2,5,5,5,1,1,4,5,4,1,1,4,2,1,4,1,2,2,5,1,1,5,1,1,3,4,4,1,2,3,1,5,5,4,1,4,1,2,1,5,1,1,1,3,4,1,1,5,1,5,1,1,5,1,1,4,3,2,4,1,4,1,5,3,3,1,5,1,3,1,1,4,1,4,5,2,3,1,1,1,1,3,1,2,1,5,1,1,5,1,1,1,1,4,1,4,3,1,5,1,1,5,4,4,2,1,4,5,1,1,3,3,1,1,4,2,5,5,2,4,1,4,5,4,5,3,1,4,1,5,2,4,5,3,1,3,2,4,5,4,4,1,5,1,5,1,2,2,1,4,1,1,4,2,2,2,4,1,1,5,3,1,1,5,4,4,1,5,1,3,1,3,2,2,1,1,4,1,4,1,2,2,1,1,3,5,1,2,1,3,1,4,5,1,3,4,1,1,1,1,4,3,3,4,5,1,1,1,1,1,2,4,5,3,4,2,1,1,1,3,3,1,4,1,1,4,2,1,5,1,1,2,3,4,2,5,1,1,1,5,1,1,4,1,2,4,1,1,2,4,3,4,2,3,1,1,2,1,5,4,2,3,5,1,2,3,1,2,2,1,4""".stripMargin

case class Fish(daysBeforeNextFish: Int) {
  def next(): Seq[Fish] = {
    if (daysBeforeNextFish == 0) {
      Seq(this.copy(daysBeforeNextFish = Fish.resetTimer), Fish.baby)
    } else {
      Seq(this.copy(daysBeforeNextFish - 1))
    }
  }
}

object Fish {
  val initialTimer = 8
  val resetTimer = 6
  val baby = Fish(initialTimer)
}

def part1(input: String) = {
  val initialState = input.split(",").map(_.toInt).map(Fish(_))
  val nbRounds = 80

  val finalState = (0 until nbRounds)
    .foldLeft(initialState) { (accState, round) =>
      accState.flatMap(_.next())
    }

  finalState.length
}

assert(part1(sample) == 5934)
assert(part1(input) == 362639)

def part2(input: String) = {
  val initialState = input.split(",").map(_.toInt).map(Fish(_))
  val nbRounds = 256

  @tailrec
  def nextIteration(acc: Seq[Fish], nbRoundsLeft: Int): Seq[Fish] = {
    if (nbRoundsLeft > 0) nextIteration(acc.flatMap(_.next()), nbRoundsLeft - 1)
    else acc
  }

  nextIteration(initialState, nbRounds).length
}


val a = part2(sample)
println(s"HUHU a=$a")
