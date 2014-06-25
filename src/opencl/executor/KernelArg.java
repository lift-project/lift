package opencl.executor;

public abstract class KernelArg {
    KernelArg(long handle) {
        nativeHandle = handle;
    }
    public native void dispose();

    protected long nativeHandle;
}
