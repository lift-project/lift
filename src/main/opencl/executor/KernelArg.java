package opencl.executor;

abstract class KernelArg extends JNIHandle {
    KernelArg(long handle) {
        super(handle);
    }
}
