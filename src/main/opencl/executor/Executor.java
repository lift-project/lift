package opencl.executor;

public class Executor {

    public static class ExecutorFailureException extends Exception {
        public ExecutorFailureException(String message){
            super(message);
            System.err.println("Runtime error in the executor");
        }

        /// Consume the error. This restarts the Executor
        public void consume(){
            System.err.print("Restarting the executor... ");

            // Shutdown
            try { Executor.shutdown(); }
            catch( Exception e) { /* ignore shutdown errors */ }

            // restart
            try { Executor.init(); }
            catch( Exception e) {
                System.err.print("Catastrophic failure, cannot restart the executor");
                throw new RuntimeException("Cannot start Execution engine");
            }
            System.err.println("[ok]");
        }
    }

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
        return execute(kernelCode, "KERNEL", "", localSize1, localSize2, localSize3,
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

    /** Compute matrix-matrix multiply natively */
    public static Float[] nativeMatrixMultiply(Float[] a, Float[] b, int n, int m, int k) {
        float[] aa = new float[n*m];
        float[] bb = new float[n*k];
        float[] cc = new float[n*m];
        Float[] out = new Float[n*m];

        for(int i = 0; i < n * m; ++i){
            aa[i] = a[i];
            bb[i] = b[i];
        }

        nativeMatrixMultiply(aa,bb,cc,n,m,k);

        for(int i = 0; i < n * m; ++i){
            out[i] = cc[i];
        }

        return out;
    }

    public native static void nativeMatrixMultiply(float[] a, float[] b, float[] out, int n, int m, int k);

    public native static double execute(String kernelCode, String kernelName, String buildOptions,
                                        int localSize1, int localSize2, int localSize3,
                                        int globalSize1, int globalSize2, int globalSize3,
                                        KernelArg[] args);

    /** Execute the given kernel source code with the given global and local size and arguments
     * <code>iterations</code> times.
     * Returns the median runtime in milliseconds.
     *
     * @return The median runtime of the kernel in milliseconds.
     */
    public static double benchmark(String kernelCode,
                                   int localSize1, int localSize2, int localSize3,
                                   int globalSize1, int globalSize2, int globalSize3,
                                   KernelArg[] args, int iterations, double timeout) {
        return benchmark(kernelCode, "KERNEL", "", localSize1, localSize2, localSize3,
                globalSize1, globalSize2, globalSize3, args, iterations, timeout);
    }

    public native static double benchmark(String kernelCode, String kernelName, String buildOptions,
                                          int localSize1, int localSize2, int localSize3,
                                          int globalSize1, int globalSize2, int globalSize3,
                                          KernelArg[] args, int iterations, double timeOut);

    public native static void init(int platformId, int deviceId);

    public native static long getDeviceLocalMemSize();

    public native static long getDeviceGlobalMemSize();

    public native static long getDeviceMaxMemAllocSize();

    public native static long getDeviceMaxWorkGroupSize();

    public native static String getPlatformName();

    public native static String getDeviceName();

    public native static String getDeviceType();

    public static void init() {
        String platform = System.getenv("APART_PLATFORM");
        String device = System.getenv("APART_DEVICE");

        int platformId = 0;
        int deviceId = 0;

        if (platform != null) {
            try {
                platformId = Integer.parseInt(platform);
            } catch (NumberFormatException e) {
                System.err.println("Invalid platform id specified, using default.");
            }
        }

        if (device != null) {
            try {
                deviceId = Integer.parseInt(device);
            } catch (NumberFormatException e) {
                System.err.println("Invalid device id specified, using default.");
            }
        }

        init(platformId, deviceId);
    }

    public native static void shutdown();

}
