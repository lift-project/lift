package opencl.executor;

public class Executor {
    public native static void execute(String kernelCode, String kernelName, int localSize, int globalSize, KernelArg[] args);

    public static void execute(String kernelCode, int localSize, int globalSize, KernelArg[] args)
    {
        execute(kernelCode, "KERNEL", localSize, globalSize, args);
    }
}
