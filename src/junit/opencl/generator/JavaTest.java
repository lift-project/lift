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
                (Param input) -> {
                    return jJoin.comp(jfun.create((Param x1) -> {
                        return jMapGlb.create(jfun.create((Param x2) -> {
                            return jMapSeq.create(jfun.create(neg::call)).call(x2);
                        })).call(x1);
                    })).comp(jfun.create((Param x) -> jSplit.create(4).call(x))).call(input);
                });

        String code = Compile.apply(negFun);
        System.out.println(code);
    }

}
