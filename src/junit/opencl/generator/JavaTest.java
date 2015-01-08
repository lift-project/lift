package junit.opencl.generator;

import opencl.ir.Float;
import org.junit.*;

import java.util.function.*;

import ir.*;
import opencl.ir.*;
import opencl.executor.*;

public class JavaTest {

    UserFunDef add = jUserFunDef.create(
            "add",
            jStringArray.create("x", "y"),
            "{ return x+y; }",
            jTypeArray.create(jFloat.getSingleton(), jFloat.getSingleton()),
            jFloat.getSingleton());

    UserFunDef plusOne = jUserFunDef.create(
            "plusOne",
            "x",
            "{ return x+1; }",
            jFloat.getSingleton(),
            jFloat.getSingleton());

    UserFunDef pair = jUserFunDef.create(
            "pair",
            "x",
            "{ Tuple t = {x, x}; return t; }",
            jFloat.getSingleton(),
            jTupleType.create(jFloat.getSingleton(), jFloat.getSingleton()));

    UserFunDef mult = jUserFunDef.create(
            "mult",
            jStringArray.create("x", "y"),
            "{ return x*y}",
            jTypeArray.create(jFloat.getSingleton(), jFloat.getSingleton()),
            jFloat.getSingleton());

    UserFunDef neg = jUserFunDef.create("neg", "x", "{ return -x; }", jFloat.getSingleton(), jFloat.getSingleton());

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

        String code = Compile.apply(pairFun);
    }

    @Test
    public void matrixPlusOne() {

        Var M = jVar.create("M");
        Var K = jVar.create("K");

        Lambda ff = jfun.create(
                row -> jMapSeq.create(plusOne).call(row));

        Function<Param, Lambda> test = a -> jfun.create((r) -> add.call(r, a));

        Function<Param, Lambda> test2 = a -> jfun.create( row -> jMapSeq.create(test.apply(a)).call(row));

        BiFunction<Param, Param, Expr> test3 = (a, b) -> jMapGlb.create(test2.apply(b)).call(a);

        Lambda f = jfun.create(
                jArrayType.create(jArrayType.create(jFloat.getSingleton(), K), M),
                jFloat.getSingleton(),
                test3
        );

        String code = Compile.apply(f);
    }

    @Test
    public void simpleCompTest() {

        FunDecl simpleComp = neg.comp(plusOne);

        Lambda1 negFun = jfun.create(
                jArrayType.create(jFloat.getSingleton(), jVar.create("N")),
                (input) -> jMapGlb.create(simpleComp).call(input));

        String code = Compile.apply(negFun);
    }
}
