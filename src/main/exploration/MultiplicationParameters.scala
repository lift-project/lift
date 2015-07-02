package exploration

import scala.sys.process._

object MultiplicationParameters {

  val separator = "------------------------------"

  def main(args: Array[String]) : Unit = {

    val platform = 0
    val device = 0
    val variant = 3

    val mSize = 1024
    val kSize = 1024
    val nSize = 1024

    val threshold = 16
    var counter = 0

    val baseline = s"--ig --il -s $nSize -s $mSize -s $kSize -p $platform -d $device -x128 -y16 --bm 8 --bn 8 -l16 -l16 --variant 3 -g128 -g128 -c"
    val script = "scripts/MatrixMultiplication"

    var doRecovery = false
    var filename = ""

    if (args.nonEmpty) {
      doRecovery = true
      filename = args(0)
    }

    var tileSizeK = 2
    while (tileSizeK <= 2048) {

      var tileSizeM = 2

      var tileSizeN = tileSizeM
      while (tileSizeM <= 2048) {

        val totalElementsInLocal = tileSizeM * tileSizeK * 2
        if (totalElementsInLocal <= 4096 && totalElementsInLocal >= 1024) {

          var workPerThreadM = 1
          while(workPerThreadM <= 64) {

            var workPerThreadN = 1
            while (workPerThreadN <= 64) {

              if (workPerThreadM * workPerThreadN <= 128 &&
                tileSizeM >= workPerThreadM && tileSizeN >= workPerThreadN ) {

                var localSizeM = tileSizeM / workPerThreadM
                var localSizeN = tileSizeN / workPerThreadN

                if (localSizeM*localSizeN <= 1024) {

                  var globalSizeN = nSize / workPerThreadN
                  for (i <- 0 until 4) {

                    var globalSizeM = mSize / workPerThreadM
                    for (i <- 0 until 4) {

                      if (globalSizeM >= localSizeM && globalSizeN >= localSizeN) {

                        if (doRecovery) {
                          recover(filename) match {
                            case (x, y, bm, bn, l1, l2, g1, g2) =>
                              tileSizeM = x
                              tileSizeK = y
                              workPerThreadM = bm
                              workPerThreadN = bn
                              localSizeM = l1
                              localSizeN = l2
                              globalSizeM = g1
                              globalSizeN = g2
                            case _ =>
                          }
                          doRecovery = false
                        }

                        val parameters = Array("--il", "-c", "-s" + mSize, "-s" + kSize, "-s" + nSize,
                          "-p" + platform, "-d" + device, "--variant", variant.toString,
                          "-x" + tileSizeM, "-y" + tileSizeK,
                          "--bm", workPerThreadM.toString, "--bn", workPerThreadN.toString,
                          "-l" + localSizeM, "-l" + localSizeN,
                          "-g" + globalSizeM, "-g" + globalSizeN).mkString(" ")

                        if (counter % threshold == 0) {
                          counter = threshold
                          println(separator)
                          println("baseline")
                          (script + " " + baseline).!
                        }

                        counter += 1

                        println(separator)
                        println(parameters)
                        (script + " " + parameters).!
                        println(separator)
                        val s = parameters + " --ig"
                        println(s)
                        (script + " " + s).!
                      }

                      globalSizeM /= 2
                    }
                    globalSizeN /= 2
                  }
                }

              }
              workPerThreadN *= 2
            }
            workPerThreadM *= 2
          }

        }
        tileSizeM *= 2
        tileSizeN = tileSizeM
      }
      tileSizeK *= 2
    }
  }

  def recover(filename: String): (Int, Int, Int, Int, Int, Int, Int, Int) = {
    var log = s"tail -n 125 $filename".!!

    var index = -1
    var findHere = Array.empty[String]
    var lastConf = ""

    do {
      index = log.lastIndexOf(separator)
      findHere = log.substring(index).split("\n")
      lastConf = findHere(1)
      log = log.substring(0, index)
    } while(lastConf.startsWith("baseline"))

    val pattern = """.*-x(\d+) -y(\d+) --bm (\d+) --bn (\d+) -l(\d+) -l(\d+) -g(\d+) -g(\d+).*""".r

    lastConf match {
      case pattern(x, y, bm, bn, l1, l2, g1, g2) =>
        (x.toInt, y.toInt, bm.toInt, bn.toInt, l1.toInt, l2.toInt, g1.toInt, g2.toInt)
    }
  }

}
