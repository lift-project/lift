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

    public static double initAndExecute(Kernel kernel,
                                        int localSize1, int localSize2, int localSize3,
                                        int globalSize1, int globalSize2, int globalSize3,
                                        KernelArg[] args)
    {
        init();
        double runtime = execute(kernel, localSize1, localSize2, localSize3,
                globalSize1, globalSize2, globalSize3, args);
        shutdown();
        return runtime;
    }

    public static void loadLibrary()
    {
        System.loadLibrary("executor-jni");
    }

    public static void loadAndInit() {
        loadLibrary();
        init();
    }

    /** Compute matrix-matrix multiply natively */
    public static float[] nativeMatrixMultiply(float[] aa, float[] bb, int n, int m, int k) {
        float[] cc = new float[n*m];
        nativeMatrixMultiply(aa,bb,cc,n,m,k);
        return cc;
    }

    public native static void nativeMatrixMultiply(float[] a, float[] b, float[] out, int n, int m, int k);

    public native static double execute(Kernel kernel,
                                        int localSize1, int localSize2, int localSize3,
                                        int globalSize1, int globalSize2, int globalSize3,
                                        KernelArg[] args);


    public native static double benchmark(Kernel kernel,
                                         int localSize1, int localSize2, int localSize3,
                                         int globalSize1, int globalSize2, int globalSize3,
                                         KernelArg[] args, int iterations, double timeOut);

    public native static double evaluate(Kernel kernel,
                                         int localSize1, int localSize2, int localSize3,
                                         int globalSize1, int globalSize2, int globalSize3,
                                         KernelArg[] args, int iterations, double timeOut);

 

    public native static void init(int platformId, int deviceId);

    public native static long getDeviceLocalMemSize();

    public native static long getDeviceGlobalMemSize();

    public native static long getDeviceMaxMemAllocSize();

    public native static long getDeviceMaxWorkGroupSize();

    public native static boolean supportsDouble();

    public native static String getPlatformName();

    public native static String getDeviceName();

    public native static String getDeviceType();

    public static void init() {
        String platform = System.getenv("LIFT_PLATFORM");
        String device = System.getenv("LIFT_DEVICE");

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
