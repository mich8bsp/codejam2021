import scala.collection.mutable
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object CJ {

  def readInputArrStr: Array[String] = StdIn.readLine().split(" ")

  def readInputArrInt: Array[Int] = readInputArrStr.map(_.toInt)

  def readInputStr: String = StdIn.readLine()

  def readInputInt: Int = StdIn.readInt()


  def runTestCase: Try[String] = {
    val Array(xStr, yStr, s) = readInputArrStr
    val X = xStr.toInt
    val Y = yStr.toInt

    val costsPerIndexAndReplacement: mutable.Map[(Int, Char), Int] = mutable.Map()
    def fillEmpty(currIdx: Int = 0,
                  fillInMapping: Map[Int, Char] = Map()): Int = {
      if (currIdx == s.length - 1) {
        0
      } else {
        val valAtCurrent = fillInMapping.getOrElse(currIdx, s(currIdx))
        val valAtNext = fillInMapping.getOrElse(currIdx + 1, s(currIdx + 1))
        lazy val costOfNextWithC = costsPerIndexAndReplacement.getOrElseUpdate((currIdx+1, 'C'), fillEmpty(currIdx+1, Map(currIdx+1 -> 'C')))
        lazy val costOfNextWithJ = costsPerIndexAndReplacement.getOrElseUpdate((currIdx+1, 'J'), fillEmpty(currIdx+1, Map(currIdx+1 -> 'J')))

        (valAtCurrent, valAtNext) match {
          case ('C', 'C') =>
            val costOfNext = costOfNextWithC
            val costOfCurrent = costOfNext
            costsPerIndexAndReplacement((currIdx, 'C')) = costOfCurrent
            costOfCurrent
          case ('J', 'J') =>
            val costOfNext = costOfNextWithJ
            val costOfCurrent = costOfNext
            costsPerIndexAndReplacement((currIdx, 'J')) = costOfCurrent
            costOfCurrent
          case ('C', 'J') =>
            val costOfNext = costOfNextWithJ
            val costOfCurrent: Int = costOfNext + X
            costsPerIndexAndReplacement((currIdx, 'C')) = costOfCurrent
            costOfCurrent
          case ('J', 'C') =>
            val costOfNext: Int = costOfNextWithC
            val costOfCurrent: Int = costOfNext + Y
            costsPerIndexAndReplacement((currIdx, 'J')) = costOfCurrent
            costOfCurrent
          case ('C', '?') =>
            val costOfCurrent = math.min(costOfNextWithC, costOfNextWithJ + X)
            costsPerIndexAndReplacement((currIdx, 'C')) = costOfCurrent
            costOfCurrent
          case ('J', '?') =>
            val costOfCurrent = math.min(costOfNextWithC + Y, costOfNextWithJ)
            costsPerIndexAndReplacement((currIdx, 'J')) = costOfCurrent
            costOfCurrent
          case ('?', 'C') =>
            val costOfNext: Int = costOfNextWithC

           if (Y < 0) {
              val costOfCurrent = costOfNext + Y
             costsPerIndexAndReplacement((currIdx, 'J')) = costOfCurrent
             costOfCurrent
           } else {
             val costOfCurrent = costOfNext
             costsPerIndexAndReplacement((currIdx, 'C')) = costOfCurrent
             costOfCurrent
            }
          case ('?', 'J') =>
            val costOfNext: Int = costOfNextWithJ

            if (X < 0) {
              val costOfCurrent = costOfNext + X
              costsPerIndexAndReplacement((currIdx, 'C')) = costOfCurrent
              costOfCurrent
            } else {
              val costOfCurrent = costOfNext
              costsPerIndexAndReplacement((currIdx, 'J')) = costOfCurrent
              costOfCurrent
            }
          case ('?', '?') => {
            val cstNextC = costOfNextWithC
            val cstNextJ = costOfNextWithJ

            val costOfCC = cstNextC
            val costOfCJ = cstNextJ + X
            val costOfJC = cstNextC + Y
            val costOfJJ = cstNextJ

            costsPerIndexAndReplacement((currIdx, 'J')) = math.min(costOfJC, costOfJJ)
            costsPerIndexAndReplacement((currIdx, 'C')) = math.min(costOfCC, costOfCJ)

            math.min(costsPerIndexAndReplacement((currIdx, 'J')), costsPerIndexAndReplacement((currIdx, 'C')))
          }
        }
      }
    }

    Try {
      if (s.length <= 1) {
        0
      } else {
        fillEmpty()
      }
    }.map(_.toString)
  }

  def main(args: Array[String]): Unit = {
    val T: Int = readInputInt
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