import scala.annotation.tailrec
import scala.io.Source

object day4 extends App {
  val List(low, high) =
    Source
      .fromFile("day4input.txt")
      .getLines()
      .flatMap(_.split("-"))
      .map(_.toInt)
      .take(2)
      .toList

  def hasEqualAdjacentPair(s: String): Boolean =
    s.toSeq.sliding(2).exists(window => window(0) == window(1))

  def isNotDecreasing(s: String): Boolean =
    s.toCharArray.map(Character.getNumericValue).sliding(2).forall {
      case Array(prev, curr) => prev <= curr
    }

  def isValidPassword(s: String) =
    hasEqualAdjacentPair(s) && isNotDecreasing(s)

  println((low to high).map(_.toString).count(isValidPassword))

  // 2 star
  def hasGroupOfLength2(s: String): Boolean = {
    val runningOccurrences = s
      .zip(s.tail)
      .scanLeft(1) { case (z, (a, b)) => if (a == b) z + 1 else 1 }

    runningOccurrences
      .appended(1)
      .containsSlice(Seq(2, 1))
  }

  println(
    (low to high)
      .map(_.toString)
      .count(
        password => isValidPassword(password) && hasGroupOfLength2(password)
      )
  )
}
