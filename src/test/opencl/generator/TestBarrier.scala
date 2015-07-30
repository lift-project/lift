package opencl.generator

import apart.arithmetic.Var
import ir._
import ir.ast.IndexFunction._
import ir.ast._
import opencl.executor.{Compile, Execute, Executor, Utils}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{Ignore, AfterClass, BeforeClass, Test}

object TestBarrier {
  @BeforeClass def before() {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after() {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

class TestBarrier {
  @Test def basicBarrier(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)

    // Barrier should be removed
//    val f = fun(
//      ArrayType(Float, new Var("N")),
//      input => Join() o MapWrg(MapLcl(id)) o Split(128) $ input
//    )

    val f = fun(
      ArrayType(Float, new Var("N")),
      input => Join()(MapWrg(MapLcl(id))(Split(128)(input)))
    )

    val code = Compile(f)
    val (output: Array[Float], _) = Execute(inputSize)(code, f, input)

    assertArrayEquals(input, output, 0.0f)
    assertFalse(code.containsSlice("barrier"))
  }

  @Test def reorderGlobalLast(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).map(_.reverse).flatten.toArray

    // Last barrier should be removed
//    val f = fun(
//      ArrayType(Float, new Var("N")),
//      input =>
//        Join() o MapWrg(
//          MapLcl(id) o Gather(reverse) o MapLcl(id)
//        ) o Split(128) $ input
//    )

    val f = fun(
      ArrayType(Float, new Var("N")),
      input =>
        Join()(MapWrg(
          fun(x => MapLcl(id)(Gather(reverse)(MapLcl(id)(x))) )
        )(Split(128)(input)))
    )

    val code = Compile(f)
    val (output: Array[Float], _) = Execute(inputSize)(code, f, input)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(1, "barrier".r.findAllMatchIn(code).length)
  }

  @Test def reorderGlobalFirst(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).map(_.reverse).flatten.toArray

    // All barriers should be removed
//    val f = fun(
//      ArrayType(Float, new Var("N")),
//      input =>
//        Join() o MapWrg(
//          MapLcl(id) o MapLcl(id) o Gather(reverse)
//        ) o Split(128) $ input
//    )

    val f = fun(
      ArrayType(Float, new Var("N")),
      input =>
        Join()(MapWrg(
         fun(x => MapLcl(id)( MapLcl(id)( Gather(reverse)(x))) )
        )(Split(128)(input)))
    )

    val code = Compile(f)
    val (output: Array[Float], _) = Execute(inputSize)(code, f, input)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(0, "barrier".r.findAllMatchIn(code).length)
  }

  @Test def reorderLocal(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).map(_.reverse).flatten.toArray

    // First barrier should be eliminated
//    val f = fun(
//      ArrayType(Float, new Var("N")),
//      input =>
//        Join() o MapWrg(
//          toGlobal(MapLcl(id)) o toLocal(MapLcl(id)) o Gather(reverse)
//        ) o Split(128) $ input
//    )

    val f = fun(
      ArrayType(Float, new Var("N")),
      input =>
        Join()(MapWrg(
          fun(x => toGlobal(MapLcl(id))(toLocal(MapLcl(id))(Gather(reverse)(x))) )
        )(Split(128)(input)))
    )

    val code = Compile(f)
    val (output: Array[Float], _) = Execute(inputSize)(code, f, input)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(1, "barrier".r.findAllMatchIn(code).length)
  }

  @Test def reorderLastLocal(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).map(_.reverse).flatten.toArray

    // No barriers should be eliminated
//    val f = fun(
//      ArrayType(Float, new Var("N")),
//      input =>
//        Join() o MapWrg(
//          toGlobal(MapLcl(id)) o Gather(reverse) o toLocal(MapLcl(id))
//        ) o Split(128) $ input
//    )

    val f = fun(
      ArrayType(Float, new Var("N")),
      input =>
        Join()(MapWrg(
          fun(x => toGlobal(MapLcl(id))( Gather(reverse) (toLocal(MapLcl(id))(x))))
        )(Split(128)(input)))
    )

    val code = Compile(f)
    val (output: Array[Float], _) = Execute(inputSize)(code, f, input)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(2, "barrier".r.findAllMatchIn(code).length)
  }

  @Test def reorderWriteLastLocal(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).map(_.reverse).flatten.toArray

    // First barrier should be eliminated
//    val f = fun(
//      ArrayType(Float, new Var("N")),
//      input =>
//        Join() o MapWrg(
//          toGlobal(Scatter(reverse) o MapLcl(id)) o toLocal(MapLcl(id))
//        ) o Split(128) $ input
//    )

    val f = fun(
      ArrayType(Float, new Var("N")),
      input =>
        Join()(MapWrg(
          fun( x => Scatter(reverse)(toGlobal(MapLcl(id))(toLocal(MapLcl(id))(x))))
        )(Split(128)(input)))
    )

    val code = Compile(f)
    val (output: Array[Float], _) = Execute(inputSize)(code, f, input)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(1, "barrier".r.findAllMatchIn(code).length)
  }


  @Test def noReorderLocal(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input

    // First barrier should be eliminated
//    val f = fun(
//      ArrayType(Float, new Var("N")),
//      input =>
//        Join() o MapWrg(
//          toGlobal(MapLcl(id)) o toLocal(MapLcl(id))
//        ) o Split(128) $ input
//    )

    val f = fun(
      ArrayType(Float, new Var("N")),
      input =>
        Join()(MapWrg(
          fun(x => toGlobal(MapLcl(id))(toLocal(MapLcl(id))(x)))
        )(Split(128)(input)))
    )

    val code = Compile(f)
    val (output: Array[Float], _) = Execute(inputSize)(code, f, input)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(1, "barrier".r.findAllMatchIn(code).length)
  }

  @Test def noLoopNoReorderLocal(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input

    // All barriers should be eliminated
    val f = fun(
      ArrayType(Float, new Var("N")),
      input =>
        Join()(MapWrg(
          fun(x => toGlobal(MapLcl(id))(toLocal(MapLcl(id))(x)))
        )(Split(128)(input)))
    )

    val inputs = Seq(input)
    val (output, _, code) = Utils.execute(f, inputs, 128, inputSize, (true, true))

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(0, "barrier".r.findAllMatchIn(code).length)
  }

  @Test def noLoopReorderLocal(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).map(_.reverse).flatten.toArray

    // All barriers should be eliminated
//    val f = fun(
//      ArrayType(Float, new Var("N")),
//      input =>
//        Join() o MapWrg(
//          toGlobal(MapLcl(id)) o toLocal(MapLcl(id)) o Gather(reverse)
//        ) o Split(128) $ input
//    )

    val f = fun(
      ArrayType(Float, new Var("N")),
      input =>
        Join()(MapWrg(
          fun(x => toGlobal(MapLcl(id))(toLocal(MapLcl(id))(Gather(reverse)(x))))
        )(Split(128)(input)))
    )

    val inputs = Seq(input)
    val (output, _, code) = Utils.execute(f, inputs, 128, inputSize, (true, true))

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(0, "barrier".r.findAllMatchIn(code).length)
  }

  @Test def noLoopReorderLastLocal(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).map(_.reverse).flatten.toArray

    // Last barrier should be eliminated
//    val f = fun(
//      ArrayType(Float, new Var("N")),
//      input =>
//        Join() o MapWrg(
//          toGlobal(MapLcl(id)) o Gather(reverse) o toLocal(MapLcl(id))
//        ) o Split(128) $ input
//    )

    val f = fun(
      ArrayType(Float, new Var("N")),
      input =>
        Join()(MapWrg(
          fun(x => toGlobal(MapLcl(id))(Gather(reverse)(toLocal(MapLcl(id))(x))))
        )(Split(128)(input)))
    )

    val inputs = Seq(input)
    val (output, _, code) = Utils.execute(f, inputs, 128, inputSize, (true, true))

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(1, "barrier".r.findAllMatchIn(code).length)
  }

  @Test def noLoopReorder2Local(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input

    // Last and middle barriers should be eliminated
//    val f = fun(
//      ArrayType(Float, new Var("N")),
//      input =>
//        Join() o MapWrg(
//          toGlobal(MapLcl(id)) o MapLcl(id) o Gather(reverse) o
//          toLocal(MapLcl(id)) o Gather(reverse)
//        ) o Split(128) $ input
//    )

    val f = fun(
      ArrayType(Float, new Var("N")),
      input =>
        Join()(MapWrg(
          fun(x => toGlobal(MapLcl(id))(MapLcl(id)(Gather(reverse)(
            toLocal(MapLcl(id))(Gather(reverse)(x))))))
        )(Split(128)(input)))
    )

    val inputs = Seq(input)
    val (output, _, code) = Utils.execute(f, inputs, 128, inputSize, (true, true))

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(1, "barrier".r.findAllMatchIn(code).length)
  }

  @Test def noLoopReorder3Local(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).map(_.reverse).flatten.toArray

    // Last barrier should be eliminated
//    val f = fun(
//      ArrayType(Float, new Var("N")),
//      input =>
//        Join() o MapWrg(
//          toGlobal(MapLcl(id)) o Gather(reverse) o
//          MapLcl(id) o Gather(reverse) o toLocal(MapLcl(id)) o Gather(reverse)
//        ) o Split(128) $ input
//    )

    val f = fun(
      ArrayType(Float, new Var("N")),
      input =>
        Join()(MapWrg(
          fun(x => toGlobal(MapLcl(id))(Gather(reverse)(MapLcl(id)(
            Gather(reverse)(toLocal(MapLcl(id))(Gather(reverse)(x)))))))
        )(Split(128)(input)))
    )

    val inputs = Seq(input)
    val (output, _, code) = Utils.execute(f, inputs, 128, inputSize, (true, true))

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(2, "barrier".r.findAllMatchIn(code).length)
  }

  @Test def loopReorder2Local(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input

    // Middle barrier should be eliminated
//    val f = fun(
//      ArrayType(Float, new Var("N")),
//      input =>
//        Join() o MapWrg(
//          toGlobal(MapLcl(id)) o MapLcl(id) o Gather(reverse) o
//          toLocal(MapLcl(id)) o Gather(reverse)
//        ) o Split(128) $ input
//    )

    val f = fun(
      ArrayType(Float, new Var("N")),
      input =>
        Join()(MapWrg(
          fun(x => toGlobal(MapLcl(id))(MapLcl(id)(
            Gather(reverse)(toLocal(MapLcl(id))(Gather(reverse)(x))))))
        )(Split(128)(input)))
    )

    val code = Compile(f)
    val (output: Array[Float], _) = Execute(128, inputSize)(f, input)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(2, "barrier".r.findAllMatchIn(code).length)
  }

  @Test def copyToLocalInZip(): Unit = {
    val inputSize = 512
    val input = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)
    val gold = (input, input).zipped.map((x, y) => (x,y).zipped.map(_+_))

    val N = Var("N")

//    val f = fun(
//      ArrayType(ArrayType(Float, N), N),
//      ArrayType(ArrayType(Float, N), N),
//      (a, b) => {
//        MapWrg(toGlobal(MapLcl(add)) o fun(pairArrays => {
//          Zip(toLocal(MapLcl(id)) $ Get(pairArrays, 0),
//          toLocal(MapLcl(id)) $ Get(pairArrays, 1))
//        })) $ Zip(a, b)
//      }
//    )

    val f = fun(
      ArrayType(ArrayType(Float, N), N),
      ArrayType(ArrayType(Float, N), N),
      (a, b) => {
        MapWrg(fun( pairArrays => toGlobal(MapLcl(add))(
          Zip(toLocal(MapLcl(id)) $ Get(pairArrays, 0), toLocal(MapLcl(id)) $ Get(pairArrays, 1)))
        ))(Zip(a, b))
      }
    )


    val code = Compile(f)

    val (output: Array[Float], _) = Execute(inputSize)(code, f, input, input)

    assertArrayEquals(gold.flatten, output, 0.0f)
    assertEquals(1, "barrier".r.findAllMatchIn(code).length)
  }

  @Test def copyToLocalInZipAndReorder(): Unit = {
    val inputSize = 512
    val input = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)
    val gold = (input, input).zipped.map((x, y) => (x,y).zipped.toArray.reverse.map(x => x._1 + x._2))

    val N = Var("N")

    //    val f = fun(
    //      ArrayType(ArrayType(Float, N), N),
    //      ArrayType(ArrayType(Float, N), N),
    //      (a, b) => {
    //        MapWrg(toGlobal(MapLcl(add)) o Gather(reverse) o fun(pairArrays => {
    //          Zip(toLocal(MapLcl(id)) $ Get(pairArrays, 0),
    //          toLocal(MapLcl(id)) $ Get(pairArrays, 1))
    //        })) $ Zip(a, b)
    //      }
    //    )

    val f = fun(
      ArrayType(ArrayType(Float, N), N),
      ArrayType(ArrayType(Float, N), N),
      (a, b) => {
        MapWrg(fun( pairArrays => toGlobal(MapLcl(add))(Gather(reverse)(
          Zip(toLocal(MapLcl(id)) $ Get(pairArrays, 0), toLocal(MapLcl(id)) $ Get(pairArrays, 1))))
        ))(Zip(a, b))
      }
    )


    val code = Compile(f)

    val (output: Array[Float], _) = Execute(inputSize)(code, f, input, input)

    assertArrayEquals(gold.flatten, output, 0.0f)
    assertEquals(2, "barrier".r.findAllMatchIn(code).length)
  }

  @Test def copyToLocalAndReorderInZip(): Unit = {
    val inputSize = 512
    val input = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)
    val gold = (input, input).zipped.map((x, y) => (x.reverse,y).zipped.map(_ + _))

    val N = Var("N")

    //    val f = fun(
    //      ArrayType(ArrayType(Float, N), N),
    //      ArrayType(ArrayType(Float, N), N),
    //      (a, b) => {
    //        MapWrg(toGlobal(MapLcl(add)) o fun(pairArrays => {
    //          Zip(Gather(reverse) o toLocal(MapLcl(id)) $ Get(pairArrays, 0),
    //          toLocal(MapLcl(id)) $ Get(pairArrays, 1))
    //        })) $ Zip(a, b)
    //      }
    //    )

    val f = fun(
      ArrayType(ArrayType(Float, N), N),
      ArrayType(ArrayType(Float, N), N),
      (a, b) => {
        MapWrg(fun( pairArrays => toGlobal(MapLcl(add))(
          Zip(Gather(reverse)(toLocal(MapLcl(id))(Get(pairArrays, 0))),
            toLocal(MapLcl(id)) $ Get(pairArrays, 1)))
        ))(Zip(a, b))
      }
    )


    val code = Compile(f)

    val (output: Array[Float], _) = Execute(inputSize)(code, f, input, input)

    assertArrayEquals(gold.flatten, output, 0.0f)
    assertEquals(2, "barrier".r.findAllMatchIn(code).length)
  }

  @Test def copyToLocalAndReorderInZip2(): Unit = {
    val inputSize = 512
    val input = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)
    val gold = (input, input).zipped.map((x, y) => (x,y.reverse).zipped.map(_ + _))

    val N = Var("N")

    //    val f = fun(
    //      ArrayType(ArrayType(Float, N), N),
    //      ArrayType(ArrayType(Float, N), N),
    //      (a, b) => {
    //        MapWrg(toGlobal(MapLcl(add)) o fun(pairArrays => {
    //          Zip(toLocal(MapLcl(id)) $ Get(pairArrays, 0),
    //          Gather(reverse) o toLocal(MapLcl(id)) $ Get(pairArrays, 1))
    //        })) $ Zip(a, b)
    //      }
    //    )

    val f = fun(
      ArrayType(ArrayType(Float, N), N),
      ArrayType(ArrayType(Float, N), N),
      (a, b) => {
        MapWrg(fun( pairArrays => toGlobal(MapLcl(add))(
          Zip(toLocal(MapLcl(id))(Get(pairArrays, 0)),
            Gather(reverse)(toLocal(MapLcl(id))(Get(pairArrays, 1)))))
        ))(Zip(a, b))
      }
    )

    val code = Compile(f)

    val (output: Array[Float], _) = Execute(inputSize)(code, f, input, input)

    assertArrayEquals(gold.flatten, output, 0.0f)
    assertEquals(2, "barrier".r.findAllMatchIn(code).length)
  }

  @Ignore
  @Test def copyToLocalAndReorderBothInZip(): Unit = {
    val inputSize = 512
    val input = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)
    val gold = (input, input).zipped.map((x, y) => (x.reverse,y.reverse).zipped.map(_ + _))

    val N = Var("N")

    //    val f = fun(
    //      ArrayType(ArrayType(Float, N), N),
    //      ArrayType(ArrayType(Float, N), N),
    //      (a, b) => {
    //        MapWrg(toGlobal(MapLcl(add)) o fun(pairArrays => {
    //          Zip(Gather(reverse) o toLocal(MapLcl(id)) $ Get(pairArrays, 0),
    //          Gather(reverse) o toLocal(MapLcl(id)) $ Get(pairArrays, 1))
    //        })) $ Zip(a, b)
    //      }
    //    )

    val f = fun(
      ArrayType(ArrayType(Float, N), N),
      ArrayType(ArrayType(Float, N), N),
      (a, b) => {
        MapWrg(fun( pairArrays => toGlobal(MapLcl(add))(
          Zip(Gather(reverse)(toLocal(MapLcl(id))(Get(pairArrays, 0))),
            Gather(reverse)(toLocal(MapLcl(id))(Get(pairArrays, 1)))))
        ))(Zip(a, b))
      }
    )

    val code = Compile(f)

    val (output: Array[Float], _) = Execute(inputSize)(code, f, input, input)

    assertArrayEquals(gold.flatten, output, 0.0f)
    assertEquals(2, "barrier".r.findAllMatchIn(code).length)
  }

  @Test def copyOneToLocalInZip(): Unit = {
    val inputSize = 512
    val input = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)
    val gold = (input, input).zipped.map((x, y) => (x,y).zipped.map(_+_))

    val N = Var("N")

    //    val f = fun(
    //      ArrayType(ArrayType(Float, N), N),
    //      ArrayType(ArrayType(Float, N), N),
    //      (a, b) => MapWrg(toGlobal(MapLcl(add)) o fun(pairArrays =>
    //        Zip(toLocal(MapLcl(id)) $ Get(pairArrays, 0), Get(pairArrays, 1))
    //      )) $ Zip(a, b)
    //    )

    val f = fun(
      ArrayType(ArrayType(Float, N), N),
      ArrayType(ArrayType(Float, N), N),
      (a, b) => MapWrg(fun( pairArrays => toGlobal(MapLcl(add))(
        Zip(toLocal(MapLcl(id)) $ Get(pairArrays, 0), Get(pairArrays, 1)))
      ))(Zip(a, b))
    )

    val code = Compile(f)

    val (output: Array[Float], _) = Execute(inputSize)(code, f, input, input)

    assertArrayEquals(gold.flatten, output, 0.0f)
    assertEquals(1, "barrier".r.findAllMatchIn(code).length)
  }
}
