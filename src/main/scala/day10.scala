import scala.annotation.tailrec
import scala.io.Source

object day10 extends App {
  val grid: Vector[Vector[Boolean]] = Source
    .fromFile("day10input.txt")
    .getLines()
    .map(_.toCharArray.map(_ == '#').toVector)
    .toVector

  val positions: Seq[(Int, Int)] = for {
    y <- 0 until grid.length
    x <- 0 until grid(0).length
  } yield (x, y)

  @tailrec
  def gcd(a: Int, b: Int): Int =
    if (b == 0) a
    else gcd(b, a % b)

  def countVisibleAsteroids(posX: Int, posY: Int): Int = {
    positions
      .filter { case (x, y) => grid(y)(x) && !(posX == x && posY == y) }
      .count {
        case (x, y) =>
          val diffX = posX - x
          val diffY = posY - y
          val stepCount = gcd(diffX.abs, diffY.abs)
          val stepX = diffX / stepCount
          val stepY = diffY / stepCount
          (1 until stepCount)
            .forall(step => !grid(y + stepY * step)(x + stepX * step))
      }
  }

  println(
    positions
      .filter { case (x, y) => grid(y)(x) }
      .map {
        case (x, y) => countVisibleAsteroids(x, y)
      }
      .max
  )

  // 2 star
  val (bestX, bestY) = positions
    .filter { case (x, y) => grid(y)(x) }
    .maxBy { case (x, y) => countVisibleAsteroids(x, y) }

  def distance(tuple: (Int, Int)): Double = tuple match {
    case (x, y) => math.pow((bestX - x), 2) + math.pow((bestY - y), 2)
  }

  def getNormalizedDiff(x: Int, y: Int): (Int, Int) = {
    val diffX = x - bestX
    val diffY = y - bestY
    val stepCount = gcd(math.abs(diffX), math.abs(diffY))
    (diffX / stepCount, diffY / stepCount)
  }

  def alternateSeqs[A](seqs: Seq[Seq[A]]): Seq[A] = {
    val longestSeqLength = seqs.map(_.length).max
    seqs
      .map(_.map(p => Some(p)))
      .map(s => s.padTo(longestSeqLength, None))
      .transpose
      .flatten
      .flatten
  }

  val (x200, y200) = alternateSeqs(
    positions
      .filter { case (x, y) => grid(y)(x) && !(x == bestX && y == bestY) }
      .groupBy((getNormalizedDiff _).tupled)
      .view
      .mapValues(_.sortBy(distance))
      .toSeq
      .sortBy {
        case ((normX, normY), _) =>
          val angle = -math.atan2(normY, -normX)
          angle + (if (angle < math.Pi / 2) math.Pi * 2 else 0)
      }
      .map(_._2)
  )(199)

  println(x200 * 100 + y200)
}
