package opencl.executor;

public class Executor {

    public static void execute(String kernelCode, int localSize, int globalSize, KernelArg[] args)
    {
        execute(kernelCode, "KERNEL", localSize, globalSize, args);
    }

    public static void initAndExecute(String kernelCode, int localSize, int globalSize, KernelArg[] args)
    {
        init();
        execute(kernelCode, localSize, globalSize, args);
        shutdown();
    }

    public static void loadLibrary()
    {
        System.loadLibrary("executor-jni");
    }

    public native static void execute(String kernelCode, String kernelName,
                                      int localSize, int globalSize, KernelArg[] args);

    public native static void init();

    public native static void shutdown();

}
