package opencl.executor;

public class Executor {

    /** Execute the given kernel source code with the given global and local size and arguments.
      * Returns the runtime in milliseconds.
      *
      * @return The runtime of the kernel in milliseconds.
      */
    public static double execute(String kernelCode,
                                 int localSize1, int localSize2, int localSize3,
                                 int globalSize1, int globalSize2, int globalSize3,
                                 KernelArg[] args)
    {
        return execute(kernelCode, "KERNEL", localSize1, localSize2, localSize3,
                globalSize1, globalSize2, globalSize3, args);
    }

    public static double initAndExecute(String kernelCode,
                                        int localSize1, int localSize2, int localSize3,
                                        int globalSize1, int globalSize2, int globalSize3,
                                        KernelArg[] args)
    {
        init();
        double runtime = execute(kernelCode, localSize1, localSize2, localSize3,
                globalSize1, globalSize2, globalSize3, args);
        shutdown();
        return runtime;
    }

    public static void loadLibrary()
    {
        System.loadLibrary("executor-jni");
    }

    public native static double execute(String kernelCode, String kernelName,
                                        int localSize1, int localSize2, int localSize3,
                                        int globalSize1, int globalSize2, int globalSize3,
                                        KernelArg[] args);

    public native static void init(int platformId, int deviceId);

    public native static void init();

    public native static void shutdown();

}
