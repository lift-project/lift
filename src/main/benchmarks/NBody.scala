package benchmarks

import lift.arithmetic.SizeVar
import ir._
import ir.ast._
import opencl.ir._
import opencl.ir.pattern._

@deprecated("Uses an old benchmark infrastructure", "")
class NBody(override val f: Seq[(String, Array[Lambda])]) extends DeprecatedBenchmark("N-Body", Seq(1024), f, 0.1f) {
  var scalaInput: Array[(Float, Float, Float, Float, Float, Float, Float)] = Array()

  override def runScala(inputs: Any*): Array[Float] = {
    val espSqr = inputs(2).asInstanceOf[Float]
    val deltaT = inputs(3).asInstanceOf[Float]


    scalaInput.map(x => {
      val acceleration = scalaInput.map(y => {
        val r = Array(0.0f, 0.0f, 0.0f)

        r(0) = x._1 - y._1
        r(1) = x._2 - y._2
        r(2) = x._3 - y._3

        val distSqr = r(0) * r(0) + r(1) * r(1) + r(2) * r(2)

        val invDist = 1.0f / math.sqrt(distSqr + espSqr).toFloat

        val s = invDist * invDist * invDist * y._7

        (s * r(0), s * r(1), s * r(2))
      }).reduce((y, z) => (y._1 + z._1, y._2 + z._2, y._3 + z._3))

      val px = x._4 * deltaT + 0.5f * acceleration._1 * deltaT * deltaT
      val py = x._5 * deltaT + 0.5f * acceleration._2 * deltaT * deltaT
      val pz = x._6 * deltaT + 0.5f * acceleration._3 * deltaT * deltaT

      (x._1 + px.toFloat,
        x._2 + py.toFloat,
        x._3 + pz.toFloat,
        x._7,
        x._4 + acceleration._1.toFloat * deltaT,
        x._5 + acceleration._2.toFloat * deltaT,
        x._6 + acceleration._3.toFloat * deltaT,
        x._7)
    }).map(_.productIterator)
      .reduce(_ ++ _).asInstanceOf[Iterator[Float]].toArray
  }

  override def generateInputs(): Seq[Any] = {
    val inputSize = inputSizes().head

    val input = Array.fill(inputSize)((util.Random.nextFloat(),
      util.Random.nextFloat(),
      util.Random.nextFloat(),
      0.0f, 0.0f, 0.0f,
      util.Random.nextFloat()))


    scalaInput = input

    val deltaT = 0.005f
    val espSqr = 500.0f

    val pos = Array.ofDim[Float](inputSize*4)
    val vel = Array.ofDim[Float](inputSize*4)

    for (i <- 0 until inputSize) {
      pos(4*i) = input(i)._1
      pos(4*i+1) = input(i)._2
      pos(4*i+2) = input(i)._3
      pos(4*i+3) = input(i)._7

      vel(4*i) = input(i)._4
      vel(4*i+1) = input(i)._5
      vel(4*i+2) = input(i)._6
      vel(4*i+3) = input(i)._7
    }

    Seq(pos, vel, espSqr, deltaT)
  }

  override protected def check(x: Float, y: Float): Boolean = {
    var diff = (x-y)/x

    if (x == 0.0f)
      diff = 0.0f

    diff.abs >= delta
  }

}

object NBody {

  val calcAccAndAccumulate =
    UserFun("calcAcc", Array("p1", "p2", "deltaT", "espSqr", "acc"),
      """|{
        |  float4 r;
        |  r.xyz = p2.xyz - p1.xyz ;
        |  float distSqr = r.x*r.x + r.y*r.y + r.z*r.z;
        |  float invDist = 1.0f / sqrt(distSqr + espSqr);
        |  float invDistCube = invDist * invDist * invDist;
        |  float s = invDistCube * p2.w;
        |  float4 res;
        |  res.xyz = acc.xyz + s * r.xyz;
        |  return res;
        |}
        | """.stripMargin,
      Seq(Float4, Float4, Float, Float, Float4), Float4)

  val calcAccNoAdd =
    UserFun("calcAcc", Array("p1", "p2", "deltaT", "espSqr"),
      """|{
        |  float4 r;
        |  r.xyz = p2.xyz - p1.xyz ;
        |  float distSqr = r.x*r.x + r.y*r.y + r.z*r.z;
        |  float invDist = 1.0f / sqrt(distSqr + espSqr);
        |  float invDistCube = invDist * invDist * invDist;
        |  float s = invDistCube * p2.w;
        |  float4 res;
        |  res.xyz = s * r.xyz;
        |  return res;
        |}
        | """.stripMargin,
      Seq(Float4, Float4, Float, Float), Float4)

  val update =
    UserFun("update", Array("pos", "vel", "deltaT", "acceleration"),
      """|{
        |  float4 newPos;
        |  newPos.xyz = pos.xyz + vel.xyz * deltaT + 0.5f * acceleration.xyz * deltaT * deltaT;
        |  newPos.w = pos.w;
        |  float4 newVel;
        |  newVel.xyz = vel.xyz + acceleration.xyz * deltaT;
        |  newVel.w = vel.w;
        |  Tuple t = {newPos, newVel};
        |  return t;
        |}
      """.stripMargin,
      Seq(Float4, Float4, Float, Float4), TupleType(Float4, Float4))

  val N = SizeVar("N")

  val amd = fun(
    ArrayTypeWSWC(Float4, N),
    ArrayTypeWSWC(Float4, N),
    Float,
    Float,
    (pos, vel, espSqr, deltaT) =>
      MapGlb(fun(p1 =>

        toGlobal(MapSeq(fun(acceleration =>
          update(Get(p1, 0), Get(p1, 1), deltaT, acceleration))))

          o ReduceSeq(fun((acc, p2) =>
          calcAccAndAccumulate(Get(p1,0), p2, deltaT, espSqr, acc)),
          Value("(float4) 0.0f", Float4)) $ pos

      )) $ Zip(pos, vel)
  )

  val tileX = 256
  val tileY = 1

  val nvidia = fun(
    ArrayTypeWSWC(Float4, N),
    ArrayTypeWSWC(Float4, N),
    Float,
    Float,
    (pos, vel, espSqr, deltaT) =>
      Join() o
        MapWrg(1)(Join() o MapWrg(0)(fun(p1Chunk => // ArrayTypeWSWC(Flat4, tileX)
          \(newP1Chunk =>
            MapLcl(1)(\(bla =>
              toGlobal(MapLcl(0)( fun( p1 =>
                NBody.update(Get(Get(p1,0), 0), Get(Get(p1, 0), 1), deltaT, Get(p1,1))
              ))) $ Zip(newP1Chunk, bla))) o
              Join() o
              ReduceSeq(fun((acc, p2) =>
                Let(p2Local =>
                  MapLcl(1)(\(accDim2 =>
                    Join() o
                      MapLcl(0)(fun(p1 => // ( (float4, float4), float4 )

                        ReduceSeq(fun((acc, p2) =>
                          NBody.calcAccAndAccumulate(Get(Get(p1,0), 0), p2, deltaT, espSqr, acc)),
                          Get(p1,1)) $ accDim2._0
                      )) $ Zip(newP1Chunk, accDim2._1)
                  )) $ Zip(p2Local, acc)
                ) o toLocal(MapLcl(1)(MapLcl(0)(idF4))) $ p2
              ), MapLcl(1)(MapLcl(0)(idF4)) $ Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(Float4, tileX), tileY))) o Split(tileY) o Split(tileX) $ pos
          ) $ Zip(toPrivate(MapLcl(idF4)) $ Get(Unzip() $ p1Chunk, 0), Get(Unzip() $ p1Chunk, 1))
        )) o Split(tileX)) o Split(N) $ Zip(pos, vel)
  )

  val lessLoadsToGlobal = fun(
    ArrayTypeWSWC(Float4, N),
    ArrayTypeWSWC(Float4, N),
    Float,
    Float,
    (pos, vel, espSqr, deltaT) =>
      MapGlb(fun(p1 =>

        fun (p1 =>
          toGlobal(MapSeq(fun(acceleration =>
            NBody.update(Get(p1, 0), Get(p1, 1), deltaT, acceleration))))

            o ReduceSeq(fun((acc, p2) =>
            NBody.calcAccAndAccumulate(Get(p1,0), p2, deltaT, espSqr, acc)),
            Value("(float4) 0.0f", Float4)) $ pos

        ) $ Tuple(toPrivate(idF4) $ Get(p1, 0), toPrivate(idF4) $ Get(p1,1))
      )) $ Zip(pos, vel)
  )


  def apply() = new NBody(Seq(
    ("amd", Array[Lambda](amd)),
    ("nvidia", Array[Lambda](nvidia)),
    ("lessLoadsToGlobal", Array[Lambda](lessLoadsToGlobal))
  ))

  def main(args: Array[String]): Unit = {
    NBody().run(args)
  }
}
