package opencl.executor;

import utils.NativeUtils;

import java.io.IOException;
import java.util.Objects;

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
        try {
            if (System.getProperty("os.name").toLowerCase().contains("mac")) {
                NativeUtils.loadLibraryFromJar("/lib/libexecutor-jni.dylib");
            } else {
                if (System.getProperty("os.name").toLowerCase().contains("windows")) {
                    NativeUtils.loadLibraryFromJar("/resources/lib/executor-jni.dll");
                } else {
                    NativeUtils.loadLibraryFromJar("/lib/libexecutor-jni.so");
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(-1);
        }
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

    /**
     * Executes the given kernel with the given runtime configuration and arguments iterations many
     * times. Returns the runtime of all runs in the order they were executed.
     * If the execution of a single run takes longer than the time specified in timeout only a
     * single iteration is performed. If the timeout is 0.0 this check will not be performed and
     * all iterations will be performed independent of the runtime of the kernel.
     *
     * @param kernel The kernel to execute
     * @param localSize1 Work-group size in dimension 0
     * @param localSize2 Work-group size in dimension 1
     * @param localSize3 Work-group size in dimension 2
     * @param globalSize1 Number of work-items in dimension 0
     * @param globalSize2 Number of work-items in dimension 1
     * @param globalSize3 Number of work-items in dimension 2
     * @param args Kernel arguments in the same order as expected by the kernel.
     * @param iterations The number of iterations to execute
     * @param timeOut A timeout specified in seconds. If the runtime of the kernel exceeds the
     *                timeout only a single iteration is executed. If the value is 0.0 this check
     *                will be disabled and all iterations will be performed independent of the
     *                kernel runtime.
     * @return An array of iterations many double values of runtimes measured using the OpenCL
     *         timing API. The ith value in the array is the runtime of the execution of the ith
     *         iteration.
     */
    public native static double[] benchmark(Kernel kernel,
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

    public native static boolean isLittleEndian();

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
