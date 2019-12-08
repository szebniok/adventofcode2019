import scala.io.Source

object day8 extends App {
  def getLayers(pixels: Vector[Int],
                width: Int,
                height: Int): Vector[Vector[Int]] =
    pixels.grouped(width * height).toVector

  val pixels: Vector[Int] =
    Source
      .fromFile("day8input.txt")
      .getLines()
      .next
      .toCharArray
      .map(Character.getNumericValue)
      .toVector

  val layerWithMinAmountOf0 = getLayers(pixels, 25, 6).minBy(_.count(_ == 0))
  println(
    layerWithMinAmountOf0
      .count(_ == 1) * layerWithMinAmountOf0.count(_ == 2)
  )

  // 2 star
  def to2D(pixels: Vector[Int], width: Int, height: Int): Vector[Vector[Int]] =
    pixels.grouped(width).toVector

  def combineLayers(above: Vector[Vector[Int]],
                    below: Vector[Vector[Int]]): Vector[Vector[Int]] =
    above.zip(below).map {
      case (aRow, bRow) =>
        aRow.zip(bRow).map {
          case (2, bPixel) => bPixel
          case (aPixel, _) => aPixel
        }
    }

  def pixelToChar(pixel: Int): Character =
    pixel match {
      case 0 => '#'
      case 1 => '.'
      case _ => ' '
    }

  println(
    getLayers(pixels, 25, 6)
      .map(to2D(_, 25, 6))
      .reduceLeft(combineLayers)
      .map(_.map(pixelToChar).mkString(""))
      .mkString("\n")
  )
}
