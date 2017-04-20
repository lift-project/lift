package opencl.executor;

public class LocalArg extends KernelArg {
    public static native LocalArg create(long size);

    LocalArg(long handle) {
        super(handle);
    }
}
