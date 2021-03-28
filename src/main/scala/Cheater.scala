import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object Cheater {

  def readInputArrStr: Array[String] = StdIn.readLine().split(" ")
  def readInputArrInt: Array[Int] = readInputArrStr.map(_.toInt)
  def readInputStr: String = StdIn.readLine()
  def readInputInt: Int = StdIn.readInt()

  def runTestCase: Try[String] = {
    val playersAnswers = (0 until 100).map(_ => readInputStr).toList
    val cheaterIdx = findCheaterIdx(playersAnswers)
    Try((cheaterIdx+1).toString)
  }

  def findCheaterIdx(playerAnswers: List[String]): Int = {
    playerAnswers.zipWithIndex.map({
      case (answer, idx) => {
        val score = answer.map({
          case '0' => 0
          case '1' => 1
        }).sum

        (score, idx)
      }
    }).maxBy(_._1)
      ._2
  }

  def main(args: Array[String]): Unit = {
    val T: Int = readInputInt
    val P: Int = readInputInt
    val totalRes = Range(1, T + 1).flatMap(testIdx => {

      val testCaseRes: Try[String] = runTestCase
      testCaseRes match {
        case Success(v) => List(s"Case #$testIdx: $v")
        case Failure(exception) => List(s"Case #$testIdx: IMPOSSIBLE")
      }
    })

    totalRes.foreach(println)
  }

}