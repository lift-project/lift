package opencl.executor;

public class Executor {

    /** Execute the given kernel source code with the given global and local size and arguments.
      * Returns the runtime in milliseconds.
      *
      * @return The runtime of the kernel in milliseconds.
      */
    public static double execute(String kernelCode, int localSize, int globalSize, KernelArg[] args)
    {
        return execute(kernelCode, "KERNEL", localSize, globalSize, args);
    }

    public static double initAndExecute(String kernelCode, int localSize, int globalSize, KernelArg[] args)
    {
        init();
        double runtime = execute(kernelCode, localSize, globalSize, args);
        shutdown();
        return runtime;
    }

    public static void loadLibrary()
    {
        System.loadLibrary("executor-jni");
    }

    public native static double execute(String kernelCode, String kernelName,
                                        int localSize, int globalSize, KernelArg[] args);

    public native static void init(int platformId, int deviceId);

    public native static void init();

    public native static void shutdown();

}
