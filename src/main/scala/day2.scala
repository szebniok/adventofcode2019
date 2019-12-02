import scala.annotation.tailrec
import scala.io.Source

object day2 extends App {
  val initialOpcodes = Source
    .fromFile("day2input.txt")
    .getLines
    .next
    .split(",")
    .map(_.toInt)
    .toVector

  val alarm1202 = initialOpcodes.updated(1, 12).updated(2, 2)

  @tailrec
  def runProgram(opcodes: Vector[Int], offset: Int = 0): Vector[Int] = {
    val registers = opcodes
    val code = opcodes.drop(offset)
    code match {
      case Vector(99, _*) => registers
      case Vector(operationCode, src1, src2, dest, _*) => {
        val updatedOpcodes =
          updateOpcodes(
            registers,
            src1,
            src2,
            dest,
            getOperationFromCode(operationCode)
          )
        runProgram(updatedOpcodes, offset + 4)
      }
    }
  }

  def updateOpcodes(registers: Vector[Int],
                    src1: Int,
                    src2: Int,
                    dest: Int,
                    op: (Int, Int) => Int): Vector[Int] =
    registers.updated(dest, op(registers(src1), registers(src2)))

  def getOperationFromCode(code: Int): (Int, Int) => Int =
    if (code == 1) _ + _
    else _ * _

  println(runProgram(alarm1202)(0))

  // 2 star
  val values: Seq[(Int, Int)] = for {
    noun <- 0 to 100
    verb <- 0 to 100
  } yield (noun, verb)

  values
    .map {
      case (noun, verb) =>
        (
          noun,
          verb,
          runProgram(initialOpcodes.updated(1, noun).updated(2, verb))(0)
        )
    }
    .filter(_._3 == 19690720)
    .foreach { case (noun, verb, _) => println(noun * 100 + verb) }

}
