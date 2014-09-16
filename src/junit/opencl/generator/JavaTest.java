package junit.opencl.generator;

import opencl.ir.Float;
import org.junit.*;

import ir.*;
import opencl.ir.*;
import opencl.executor.*;

public class JavaTest {

    @BeforeClass
    public static void before() {
        Executor.loadLibrary();
        Executor.init();
    }

    @AfterClass
    public static void after() {
        Executor.shutdown();
    }

    @Test
    public void vectorNegSimple() {
        UserFunDef neg = jUserFunDef.create("neg", "x", "{ return -x; }", jFloat.getSingleton(), jFloat.getSingleton());

        Lambda1 negFun = jfun.create(
                jArrayType.create(jFloat.getSingleton(), jVar.create("N")),
                (input) -> {
                    return jJoin.comp(jfun.create((Param x1) -> {
                        return jMapGlb.create(jfun.create((Param x2) -> {
                            return jMapSeq.create(neg).call(x2);
                        })).call(x1);
                    })).comp(jfun.create((Param x) -> jSplit.create(4).call(x))).call(input);
                });

        String code = Compile.apply(negFun);

        Lambda negFun2 = jfun.create(
                jArrayType.create(jFloat.getSingleton(), jVar.create("N")),
                (input) -> {
                    return jJoin.comp(jMapGlb.create(
                                    jMapSeq.create(neg))
                           ).comp(jSplit.create(4)).call(input);
                });

        String code2 = Compile.apply(negFun2);
    }

    @Test
    public void vectorScalarMultiplication() {
        UserFunDef mult = jUserFunDef.create(
                "mult",
                jStringArray.create("x", "y"),
                "{ return x*y}",
                jTypeArray.create(jFloat.getSingleton(), jFloat.getSingleton()),
                jFloat.getSingleton());

        Lambda multFun = jfun.create(
                jArrayType.create(jFloat.getSingleton(), jVar.create("N")),
                jFloat.getSingleton(),
                (input, alpha) -> {
                    return jMapGlb.create(
                            jfun.create((x) -> mult.call(alpha, x)))
                            .call(input);
                });

        String code = Compile.apply(multFun);
    }

    @Test
    public void vectorPair() {
        UserFunDef pair = jUserFunDef.create(
                "pair",
                "x",
                "{ Tuple t = {x, x}; return t; }",
                jFloat.getSingleton(),
                jTupleType.create(jFloat.getSingleton(), jFloat.getSingleton()));

        Lambda pairFun = jfun.create(
                jArrayType.create(jFloat.getSingleton(), jVar.create("N")),
                (input) -> {
                    return jJoin.comp(jMapWrg.create(
                            jJoin.comp(jMapLcl.create(
                                    jMapSeq.create(pair)
                            )).comp(jSplit.create(4))
                    )).comp(jSplit.create(1024)).call(input);
                }
        );
    }
    /*
    @Test def VECTOR_PAIR() {
        val inputSize = 1024
        val inputArray = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

        val gold = inputArray.map((f) => Array(f, f)).flatten

        val pair = UserFunDef("pair", "x", "{ Tuple t = {x, x}; return t; }", Float, TupleType(Float, Float))

        val pairFun = fun(ArrayType(Float, Var("N")), (input) =>
                Join() o MapWrg(
                Join() o MapLcl(MapSeq(pair)) o Split(4)
        ) o Split(1024) o input
        )

        val (output, runtime) = Execute(inputArray.length)(pairFun, inputArray, inputArray.size)

        (gold, output).zipped.map(assertEquals(_,_,0.0))

        println("output(0) = " + output(0))
        println("runtime = " + runtime)
    }
    */
}
