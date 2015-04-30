package ir.interop;

@FunctionalInterface
public interface QuadFunction<A,B,C,D,R> {
    R apply(A a, B b, C c, D d);
}
