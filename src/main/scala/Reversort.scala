import scala.collection.mutable
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object Reversort {

  def readInputArrStr: Array[String] = StdIn.readLine().split(" ")
  def readInputArrInt: Array[Int] = readInputArrStr.map(_.toInt)
  def readInputStr: String = StdIn.readLine()
  def readInputInt: Int = StdIn.readInt()

  def runTestCase: Try[String] = {
    val n = readInputInt
    val arr = readInputArrInt.zipWithIndex

    val mutArr = mutable.Buffer[(Int, Int)]()
    mutArr.appendAll(arr)

    var cost = 0
    (1 until mutArr.length).foreach(i => {
      val j = mutArr.slice(i-1, mutArr.length).minBy(_._1)._2 + 1
      cost = cost + (j - i + 1)
      var start = i-1
      var end = j-1
      while(start < end){
        val temp = mutArr(start)
        mutArr(start) = (mutArr(end)._1, mutArr(start)._2)
        mutArr(end) = (temp._1, mutArr(end)._2)
        start +=1
        end -=1
      }
    })

    Success(cost.toString)
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