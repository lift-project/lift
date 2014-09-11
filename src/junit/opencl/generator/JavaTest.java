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
        UserFunDef neg = new UserFunDef("neg", "x", "{ return -x; }", jFloat.getSingleton(), jFloat.getSingleton());

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
        UserFunDef mult = new UserFunDef("mult", new String[] {"x", "y"}, "{ return x*y}", jTupleType.create(jFloat.getSingleton(), jFloat.getSingleton()), jFloat.getSingleton());

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

}
