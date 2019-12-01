import scala.io.Source

object day1 extends App {
  println(Source.fromFile("day1input.txt").getLines().map(_.toInt).map(_ / 3 - 2).sum)

  // 2 star
  def getFuelRequirements(fuel: Int) =
    LazyList.from(fuel, 0).scanLeft(fuel)((prev, _) => prev / 3 - 2).tail.takeWhile(_ > 0).sum

  println(Source.fromFile("day1input.txt").getLines().map(_.toInt).map(getFuelRequirements).sum)
}
