import scala.io.Source

object day6 extends App {
  case class SpaceObject(id: String, var satellites: List[SpaceObject]) {
    def countOrbits(depth: Int = 0): Int =
      satellites.map(_.countOrbits(depth + 1)).sum + depth

    def lowestCommonAncestor(obj1: SpaceObject,
                             obj2: SpaceObject): Option[SpaceObject] =
      for {
        p1 <- path(obj1)
        p2 <- path(obj2)
      } yield p1.zip(p2).takeWhile(p => p._1 == p._2).last._1

    def path(spaceObject: SpaceObject): Option[List[SpaceObject]] =
      if (this == spaceObject) Some(List(this))
      else
        satellites
          .flatMap(_.path(spaceObject))
          .headOption
          .map(_.prepended(this))
  }

  val spaceObjects: Map[String, SpaceObject] = Source
    .fromFile("day6input.txt")
    .getLines()
    .map(_.split(raw"\)"))
    .map {
      case Array(spaceObjectId, satelliteId) => (spaceObjectId, satelliteId)
    }
    .toVector
    .foldLeft(Map[String, SpaceObject]()) {
      case (acc, (spaceObjectId, satelliteId)) =>
        val satellite =
          acc.getOrElse(satelliteId, SpaceObject(satelliteId, List()))
        val spaceObject =
          acc.getOrElse(spaceObjectId, SpaceObject(spaceObjectId, List()))

        spaceObject.satellites +:= satellite
        acc
          .updated(spaceObjectId, spaceObject)
          .updated(satelliteId, satellite)
    }

  println(spaceObjects("COM").countOrbits())

  // 2 star
  val lca: SpaceObject =
    spaceObjects("COM")
      .lowestCommonAncestor(spaceObjects("YOU"), spaceObjects("SAN"))
      .head

  println((for {
    youPath <- lca.path(spaceObjects("YOU"))
    sanPath <- lca.path(spaceObjects("SAN"))
  } yield (youPath.length + sanPath.length - 4)).head)
}
