package junit.opencl.generator;

import static org.junit.Assert.*;
import org.junit.*;

import java.util.*;
import java.util.function.*;
import java.util.stream.Collectors;

import ir.*;
import opencl.ir.*;
import opencl.executor.*;
import scala.collection.JavaConversions;

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

    UserFunDef distance = jUserFunDef.create("dist", jStringArray.create("x", "y", "a", "b", "id"), "{ Tuple t = {(x - a) * (x - a) + (y - b) * (y - b), id}; return t; }", jTypeArray.create(jFloat.getSingleton(), jFloat.getSingleton(), jFloat.getSingleton(), jFloat.getSingleton(), jInt.getSingleton()), jTupleType.create(jFloat.getSingleton(), jInt.getSingleton()));
    UserFunDef minimum = jUserFunDef.create("minimum", jStringArray.create("x", "y"), "{ return x._0 < y._0 ? x : y; }", jTypeArray.create(jTupleType.create(jFloat.getSingleton(), jInt.getSingleton()), jTupleType.create(jFloat.getSingleton(), jInt.getSingleton())), jTupleType.create(jFloat.getSingleton(), jInt.getSingleton()));
    UserFunDef getSecond = jUserFunDef.create("getSecond", "x", "{ return (float) x._1; }", jTupleType.create(jFloat.getSingleton(), jInt.getSingleton()), jFloat.getSingleton());


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
    public void vectorNegSimpleWithoutJfun() {
        MapGlb mg = jMapGlb.create(neg);

        Type arrayType = jArrayType.create(jFloat.getSingleton(), jVar.create("V"));

        Param p = Param.apply(arrayType);

        Lambda f = new Lambda(new Param[]{p}, mg.call(p));

        Compile.apply(f);
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
    public void vectorScalarMultiplicationWithoutJfun() {
        Type arrayType = jArrayType.create(jFloat.getSingleton(), jVar.create("N"));
        Type floatType = jFloat.getSingleton();

        Param p0 = Param.apply(arrayType);
        Param p1 = Param.apply(floatType);

        Param[] params = {p0, p1};

        Param undefParam = Param.apply(UndefType$.MODULE$);
        Expr multExpr = mult.apply(JavaConversions.asScalaBuffer(Arrays.asList(p1, undefParam)));
        Lambda1 multLambda = new Lambda1(new Param[]{undefParam}, multExpr);
        MapGlb mg = MapGlb$.MODULE$.apply(multLambda);


        Lambda f = new Lambda(params, mg.apply(JavaConversions.asScalaBuffer(Arrays.asList(params[0]))));

        Compile.apply(f);
    }

    @Test
    public void kmeans() {
        // kmeans
        Var n = jVar.create("N");
        Var k = jVar.create("K");

        // @formatter:off
        Lambda fun = jfun.create(
                jArrayType.create(jFloat.getSingleton(), n),
                jArrayType.create(jFloat.getSingleton(), n),
                jArrayType.create(jFloat.getSingleton(), k),
                jArrayType.create(jFloat.getSingleton(), k),
                jArrayType.create(jInt.getSingleton(), k),
                (x, y, a, b, i) ->
                        jMapGlb.create(
                                jfun.create(xy ->
                                                jMapSeq.create(getSecond).comp(
                                                        jReduceSeq.create(minimum, Expr.Tuple2ToValue(new scala.Tuple2<>(java.lang.Float.MAX_VALUE, -1))).comp(
                                                                jMapSeq.create(jfun.create(abi -> distance.apply(
                                                                        JavaConversions.asScalaBuffer(Arrays.asList(Get.apply(xy, 0), Get.apply(xy, 1),
                                                                                Get.apply(abi, 0), Get.apply(abi, 1), Get.apply(abi, 2))))))
                                                        )
                                                ).call(jZip.call(Arrays.asList(a, b, i)))
                                )
                        ).call(jZip.call(x, y))
        );

        String code1 = Compile.apply(fun);
    }

    @Test
    public void kmeansWithoutJfun() {
        Var n = jVar.create("N");
        Var k = jVar.create("K");

        Type[] types = {
                jArrayType.create(jFloat.getSingleton(), n),
                jArrayType.create(jFloat.getSingleton(), n),
                jArrayType.create(jFloat.getSingleton(), k),
                jArrayType.create(jFloat.getSingleton(), k),
                jArrayType.create(jInt.getSingleton(), k)};

        List<Expr> params = Arrays.asList(types).stream().map(Param::apply).collect(Collectors.toList());

        Expr zip3 = jZip.call(params.subList(2, 5));
        Expr zip2 = jZip.call(params.subList(0, 2));
        Param undef1 = Param.apply(UndefType$.MODULE$);
        Param undef2 = Param.apply(UndefType$.MODULE$);

        Expr distExpr = distance.apply(JavaConversions.asScalaBuffer(Arrays.asList(Get.apply(undef1, 0), Get.apply(undef1, 1),
                Get.apply(undef2, 0), Get.apply(undef2, 1), Get.apply(undef2, 2))));

        Lambda1 lmap1 = new Lambda1(new Param[]{undef2}, distExpr);

        MapSeq map1 = MapSeq$.MODULE$.apply(lmap1);


        MapSeq map2 = jMapSeq.create(getSecond);
        Lambda1 reduce = jReduceSeq.create(minimum, Expr.Tuple2ToValue(new scala.Tuple2<>(java.lang.Float.MAX_VALUE, -1)));

        FunCall f = map2.comp(reduce).comp(map1).call(zip3);

        Lambda1 l = new Lambda1(new Param[]{undef1}, f);
        MapGlb mg = MapGlb$.MODULE$.apply(l);

        Lambda function = new Lambda(params.toArray(new Param[0]), mg.call(zip2));

        String code2 = Compile.apply(function);
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
    public void matrixMatrix() {

        Var N = jVar.create("N");
        Var M = jVar.create("M");
        Var K = jVar.create("K");
        Var L = jVar.create("L");


        Type[] types = {
            jArrayType.create((jArrayType.create(jFloat.getSingleton(), M)), N),
                jArrayType.create((jArrayType.create(jFloat.getSingleton(), K)), L)
        };

        List<Expr> params = Arrays.asList(types).stream().map(Param::apply).collect(Collectors.toList());


        Param undef0 = Param.apply();
        Param undef1 = Param.apply();
        Param undef2 = Param.apply();

        Expr multExpr = mult.apply(JavaConversions.asScalaBuffer(Arrays.asList(Get.apply(undef2, 0), Get.apply(undef2, 1))));
        Lambda1 multLambda = new Lambda1(new Param[]{undef2}, multExpr);
        Lambda map = Lambda.FunDefToLambda(jMapSeq.create(multLambda));

        Lambda1 reduce = jReduceSeq.create(add, Expr.FloatToValue(0.0f));

        Expr zip2 = jZip.call(undef0, undef1);

        FunCall f = reduce.comp(map).call(zip2);

        Lambda1 l = new Lambda1(new Param[]{undef1}, f);
        Lambda ms = Lambda.FunDefToLambda(jMapSeq.create(l));

        Lambda1 l2 = new Lambda1(new Param[]{undef0}, ms.call(params.get(1)));

        Lambda mg = Lambda.FunDefToLambda(jMapGlb.create(l2));

        Lambda function = new Lambda(params.toArray(new Param[0]), mg.call(params.get(0)));

        Compile.apply(function);

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

    @Test
    public void composeUserFunctionWithPattern() {

        Lambda function = jfun.create(
                jArrayType.create(jArrayType.create(jFloat.getSingleton(), jVar.create("M")), jVar.create("N")),
                (input) -> jMapGlb.create(
                        jfun.create(row -> jMapSeq.create(neg).comp(jReduceSeq.create(add, Expr$.MODULE$.FloatToValue(0.0f))).call(row))
                ).call(input));

        String code = Compile.apply(function);
    }
}
