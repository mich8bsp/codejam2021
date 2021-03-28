import scala.collection.mutable
import scala.io.StdIn

object MedianSort {

  def readInputArrStr: Array[String] = StdIn.readLine().split(" ")

  def readInputArrInt: Array[Int] = readInputArrStr.map(_.toInt)

  def readInputStr: String = StdIn.readLine()

  def readInputInt: Int = StdIn.readInt()

  def runTestCase(N: Int, Q: Int): Unit = {
    val bf = mutable.ArrayBuffer[Int]()
    (1 to (N - 2)).foreach(i => {
      if (bf.isEmpty) {
        println(s"${i} ${i + 1} ${i + 2}")
      } else {
        println(s"${i + 2} ${bf.head} ${bf.last}")
      }
      val L = readInputInt
      if (L < 0) {
        System.exit(1)
      }
      if (bf.isEmpty) {
        bf.append(L)
        val others = Seq(i, i + 1, i + 2).filterNot(_ == L)
        bf.append(others.last)
        bf.prepend(others.head)
      } else {
        val head = bf.head
        val last = bf.last
        if (L == head) {
          bf.prepend(i + 2)
        } else if (L == last) {
          bf.append(i + 2)
        } else if (bf.length < 4) {
          var startCheckIdx = 1
          var stopCheckIdx = if (bf.length - 2 > startCheckIdx) bf.length - 2 else bf.length - 1
          var inserted = false
          while ((startCheckIdx < stopCheckIdx) && !inserted) {
            println(s"${i + 2} ${bf(startCheckIdx)} ${bf(stopCheckIdx)}")
            val L = readInputInt
            if (L < 0) {
              System.exit(1)
            }
            if (L == bf(stopCheckIdx)) {
              bf.insert(stopCheckIdx + 1, i + 2)
              inserted = true
            } else if (L == bf(startCheckIdx)) {
              bf.insert(startCheckIdx, i + 2)
              inserted = true
            } else {
              startCheckIdx += 1
              if (stopCheckIdx - 1 > startCheckIdx) {
                stopCheckIdx -= 1
              }
            }
          }
          if (!inserted) {
            bf.insert(startCheckIdx, i + 2)
          }
        } else {
          var startCheckIdx = 1
          var stopCheckIdx = bf.length - 2
          var inserted = false
          while ((startCheckIdx < stopCheckIdx) && !inserted) {
            if (stopCheckIdx - startCheckIdx == 1) {
              println(s"${i + 2} ${bf(startCheckIdx)} ${bf(stopCheckIdx)}")
              val L = readInputInt
              if (L < 0) {
                System.exit(1)
              }
              if (L == bf(startCheckIdx)) {
                bf.insert(startCheckIdx, i + 2)
              } else if (L == bf(stopCheckIdx)) {
                bf.insert(stopCheckIdx + 1, i + 2)
              } else {
                bf.insert(startCheckIdx + 1, i + 2)
              }
              inserted = true
            } else {
              val mid = (startCheckIdx + stopCheckIdx) / 2
              println(s"${i + 2} ${bf(startCheckIdx)} ${bf(mid)}")
              val L = readInputInt
              if (L < 0) {
                System.exit(1)
              }
              if (L == bf(mid)) {
                startCheckIdx = mid
              } else {
                stopCheckIdx = mid
              }
            }
          }
          if(!inserted){
            bf.insert(startCheckIdx, i + 2)
          }
        }
      }
    })
    println(bf.mkString(" "))
    val res = readInputInt
    if (res != 1) {
      System.exit(1)
    }
  }

  def main(args: Array[String]): Unit = {
    val Array(t, n, q): Array[Int] = readInputArrInt
    Range(1, t + 1).foreach(_ => {
      if (n < 3) {
        println((1 to n).mkString(" "))
        val res = readInputInt
        if (res != 1) {
          System.exit(1)
        }
      } else {
        runTestCase(n, q)
      }
    })
  }

}