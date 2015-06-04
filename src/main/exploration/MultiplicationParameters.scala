package exploration

import sys.process._

object MultiplicationParameters {

  def main(args: Array[String]) : Unit = {
    val platform = 1
    val device = 0
    val variant = 3

    val mSize = 1024
    val kSize = 1024
    val nSize = 1024

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

                val localSizeM = tileSizeM / workPerThreadM
                val localSizeN = tileSizeN / workPerThreadN

                if (localSizeM*localSizeN <= 1024) {

                  var globalSizeN = nSize / workPerThreadN
                  for (i <- 0 until 4) {

                    var globalSizeM = mSize / workPerThreadM
                    for (i <- 0 until 4) {

                      if (globalSizeM >= localSizeM && globalSizeN >= localSizeN) {

                        val parameters = Array("--il", "-s" + mSize, "-s" + kSize, "-s" + nSize,
                          "-p" + platform, "-d" + device, "-x" + tileSizeM, "-y" + tileSizeK,
                          "--bm", workPerThreadM.toString, "--bn", workPerThreadN.toString,
                          "-l" + localSizeM, "-l" + localSizeN,
                          "--variant", variant.toString, "-g" + globalSizeM, "-g" + globalSizeN, "-c").mkString(" ")

                        println("------------------------------")
                        println(parameters)
                        ("scripts/MatrixMultiplication " + parameters).!
                        println("------------------------------")
                        val s = parameters + " --ig"
                        println(s)
                        ("scripts/MatrixMultiplication " + s).!
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

}
