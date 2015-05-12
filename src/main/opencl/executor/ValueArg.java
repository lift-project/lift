package opencl.executor;

public class ValueArg extends KernelArg {
    public static native ValueArg create(float value);
    public static native ValueArg create(int value);


    ValueArg(long handle) {
        super(handle);
    }
}
