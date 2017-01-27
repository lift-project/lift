package opencl.executor;

public class Kernel extends JNIHandle {
    public static native Kernel create(String kernelCode, String kernelName, String buildOptions);

    Kernel(long handle) {
        super(handle);
    }

    public native void build();
}
