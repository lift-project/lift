package opencl.executor;

abstract public class JNIHandle {
    JNIHandle(long handle) {
        nativeHandle = handle;
    }

    public native void dispose();

    @SuppressWarnings({"unused", "FieldCanBeLocal"})
    private final long nativeHandle;
}
