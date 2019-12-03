import scala.io.Source

object day3 extends App {
  sealed trait Direction
  final object Up extends Direction
  final object Down extends Direction
  final object Left extends Direction
  final object Right extends Direction
  object Direction {
    def fromChar(c: Char): Direction =
      c match {
        case 'U' => Up
        case 'D' => Down
        case 'L' => Left
        case 'R' => Right
      }

    def getOffset(direction: Direction): (Int, Int) =
      direction match {
        case Up    => (0, 1)
        case Down  => (0, -1)
        case Left  => (-1, 0)
        case Right => (1, 0)
      }
  }

  case class Path(direction: Direction, distance: Int) {
    def getOffsets(): Array[(Int, Int)] =
      Array.fill(distance)(Direction.getOffset(direction))
  }
  object Path {
    def fromString(s: String): Path =
      Path(Direction.fromChar(s(0)), s.drop(1).toInt)
  }

  def manhattanDistance(a: (Int, Int), b: (Int, Int)): Int =
    math.abs(a._1 - b._1) + math.abs(a._2 - b._2)

  def getPositions(paths: Array[Path]): Array[(Int, Int)] =
    paths
      .flatMap(_.getOffsets())
      .scanLeft((0, 0))((z, offset) => (z._1 + offset._1, z._2 + offset._2))

  val Seq(firstPath, secondPath) = Source
    .fromFile("day3input.txt")
    .getLines
    .map(_.split(","))
    .map(line => line.map(Path.fromString))
    .toSeq

  val firstPathPositionsSet = getPositions(firstPath).toSet
  val secondPathPositions = getPositions(secondPath)

  println(
    secondPathPositions
      .filter(firstPathPositionsSet.contains)
      .map(manhattanDistance(_, (0, 0)))
      .drop(1) // drop origin point
      .min
  )

  // 2 star
  val firstPathPositions = getPositions(firstPath)

  val intersections = secondPathPositions.filter(firstPathPositionsSet.contains)

  println(
    intersections
      .filter(_ != (0, 0)) // drop origin point
      .map(
        pos =>
          firstPathPositions.indexOf(pos) + secondPathPositions.indexOf(pos)
      )
      .min
  )

}
