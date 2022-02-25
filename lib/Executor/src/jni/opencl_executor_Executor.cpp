#include <string>
#include <vector>
#include <limits>
#include <iostream>
#include <algorithm>
#include <thread>
#include <cassert>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Weffc++"
#include "opencl_executor_Executor.h"
#pragma GCC diagnostic pop
#include "Handle.h"
#include "Executor.h"
#include "util/Logger.h"

enum class Mode {
  Execute,
  Evaluate
};

jdouble
  executeOrEvaluate(JNIEnv* env, jclass,
                    jobject jKernel,
                    jint localSize1, jint localSize2, jint localSize3,
                    jint globalSize1, jint globalSize2, jint globalSize3,
                    jobjectArray jArgs,
                    Mode mode,
                    jint iterations,
                    jdouble timeout)
{
  double runtime = 0;

  try {
    
    auto kernel = getHandle<executor::Kernel>(env, jKernel);

    std::vector<executor::KernelArg*> args(env->GetArrayLength(jArgs));
    int i = 0;
    for (auto& p : args) {
      auto obj = env->GetObjectArrayElement(jArgs, i);
      p = getHandle<executor::KernelArg>(env, obj);
      ++i;
    }


    switch(mode) {
      case Mode::Execute:
        runtime = execute(*kernel,
          localSize1, localSize2, localSize3, globalSize1, globalSize2, globalSize3, args);
        break;
      case Mode::Evaluate:
        runtime = evaluate(*kernel,
          localSize1, localSize2, localSize3, globalSize1, globalSize2, globalSize3,
          args, iterations, timeout);
        break;
      default:
        return 0;
    }

  } catch(cl::Error err) {
    jclass jClass = env->FindClass("opencl/executor/Executor$ExecutorFailureException");
    if(!jClass) LOG_ERROR("[JNI ERROR] Cannot find the exception class");
    env->ThrowNew(jClass, 
        (std::string("Executor failure: ") + err.what() + std::string(". Error code: ") + 
         executor::logger_impl::getErrorString(err.err())).c_str());
  } catch(...) {
    jclass jClass = env->FindClass("opencl/executor/Executor$ExecutorFailureException");
    if(!jClass) LOG_ERROR("[JNI ERROR] Cannot find the exception class");
    env->ThrowNew(jClass, "Executor failure");
  }

  return runtime;
}

void 
  Java_opencl_executor_Executor_nativeMatrixMultiply(JNIEnv *env, jclass,
                                                     jfloatArray a, 
                                                     jfloatArray b,
                                                     jfloatArray out,
                                                     jint N, jint, jint)
{
  float *aa = static_cast<float*>(env->GetFloatArrayElements( a, NULL));
  float *bb = static_cast<float*>(env->GetFloatArrayElements( b, NULL));
  float *cc = static_cast<float*>(env->GetFloatArrayElements( out, NULL));

  /*for (int i=0; i<N; i++) {

    float kk[N];
    for (int j=0; j<N; j++) 
      kk[j] = 0;

    for (int k=0; k<N; k++)
      for (int j=0; j<N; j++)
        kk[j] += aa[i*N+k] * bb[k*N+j];

    for (int j=0; j<N; j++)
       cc[i*N+j] = kk[j];
  }*/
  std::vector < std::thread > threads;

  std::vector<float> kk(N);
  auto mmult = [&](int from, int to) {
    for (int i=from; i<to; i++) {
      std::fill(kk.begin(), kk.end(), 0.0f);

      for (int k=0; k<N; k++)
        for (int j=0; j<N; j++)
          kk[j] += aa[i*N+k] * bb[k*N+j];

      for (int j=0; j<N; j++)
         cc[i*N+j] = kk[j];
    }
  };

  int nthreads = std::thread::hardware_concurrency();
  int chunk = N / nthreads;
  for (auto tid = 0; tid < nthreads; tid++) {
    threads.push_back(std::thread([=]{mmult(tid*chunk, (tid+1)*chunk);}));
  }
  for (auto & t : threads) t.join();


  env->ReleaseFloatArrayElements(a, aa, 0);
  env->ReleaseFloatArrayElements(b, bb, 0);
  env->ReleaseFloatArrayElements(out, cc, 0); 
}

jdouble
  Java_opencl_executor_Executor_execute(JNIEnv* env, jclass jClass,
                                        jobject jKernel,
                                        jint localSize1, jint localSize2, jint localSize3,
                                        jint globalSize1, jint globalSize2, jint globalSize3,
                                        jobjectArray jArgs)
{
  return executeOrEvaluate(env, jClass, jKernel,
        localSize1, localSize2, localSize3, globalSize1, globalSize2, globalSize3,
        jArgs, Mode::Execute, 0, 0.0);
}

jdoubleArray
  Java_opencl_executor_Executor_benchmark(
    JNIEnv* env,
    jclass,
    jobject jKernel,
    jint localSize1, jint localSize2, jint localSize3,
    jint globalSize1, jint globalSize2, jint globalSize3,
    jobjectArray jArgs,
    jint iterations,
    jdouble timeout)
{
  std::vector<double> runtimes;

  try {
    
    auto kernel = getHandle<executor::Kernel>(env, jKernel);

    std::vector<executor::KernelArg*> args(env->GetArrayLength(jArgs));
    int i = 0;
    for (auto& p : args) {
      auto obj = env->GetObjectArrayElement(jArgs, i);
      p = getHandle<executor::KernelArg>(env, obj);
      ++i;
    }

    benchmark(*kernel,
      localSize1, localSize2, localSize3, globalSize1, globalSize2, globalSize3,
      args, iterations, timeout, runtimes);

  } catch(cl::Error err) {
    jclass jClass = env->FindClass("opencl/executor/Executor$ExecutorFailureException");
    if(!jClass) LOG_ERROR("[JNI ERROR] Cannot find the exception class");
    env->ThrowNew(jClass, (std::string("Executor failure: ") + err.what()).c_str());
  } catch(...) {
    jclass jClass = env->FindClass("opencl/executor/Executor$ExecutorFailureException");
    if(!jClass) LOG_ERROR("[JNI ERROR] Cannot find the exception class");
    env->ThrowNew(jClass, "Executor failure");
  }

  if (runtimes.size() > 0) {
    jdoubleArray jRuntimes;
    jRuntimes = env->NewDoubleArray(runtimes.size());
    env->SetDoubleArrayRegion(jRuntimes, 0, runtimes.size(), runtimes.data());
    return jRuntimes;
  } else {
    jclass jClass = env->FindClass("opencl/executor/Executor$ExecutorFailureException");
    if(!jClass) LOG_ERROR("[JNI ERROR] Cannot find the exception class");
    env->ThrowNew(jClass, "Executor failure: No runtimes recorded");
    return nullptr;
  }
}

jdouble
  Java_opencl_executor_Executor_evaluate(JNIEnv* env, jclass jClass,
                                        jobject jKernel,
                                        jint localSize1, jint localSize2, jint localSize3,
                                        jint globalSize1, jint globalSize2, jint globalSize3,
                                        jobjectArray jArgs,
                                        jint iterations,
                                        jdouble timeout)
{
  return executeOrEvaluate(env, jClass, jKernel,
        localSize1, localSize2, localSize3, globalSize1, globalSize2, globalSize3,
        jArgs, Mode::Evaluate, iterations, timeout);
}



void Java_opencl_executor_Executor_init(JNIEnv *, jclass,
                                        jint platformId,
                                        jint deviceId)
{
  initExecutor(platformId, deviceId);
}

jstring Java_opencl_executor_Executor_getPlatformName(JNIEnv * env, jclass)
{
  auto name = getPlatformName();
  return env->NewStringUTF(name.c_str());
}

jlong Java_opencl_executor_Executor_getDeviceLocalMemSize(JNIEnv *, jclass)
{
  const auto value = static_cast<unsigned long long>(getDeviceLocalMemSize());
  ASSERT_MESSAGE(value <= (std::numeric_limits<unsigned long long>::max)(), "JNI cast overflow");
  return value;
}

jlong Java_opencl_executor_Executor_getDeviceGlobalMemSize(JNIEnv *, jclass)
{
  const auto value =  static_cast<unsigned long long>(getDeviceGlobalMemSize());
  ASSERT_MESSAGE(value <= (std::numeric_limits<unsigned long long>::max)(), "JNI cast overflow");
  return value;
}

jlong Java_opencl_executor_Executor_getDeviceMaxMemAllocSize(JNIEnv *, jclass)
{
  const auto value = static_cast<unsigned long long>(getDeviceMaxMemAllocSize());
  ASSERT_MESSAGE(value <= (std::numeric_limits<unsigned long long>::max)(), "JNI cast overflow");
  return value;
}

jlong Java_opencl_executor_Executor_getDeviceMaxWorkGroupSize(JNIEnv *, jclass)
{
  const auto value = static_cast<unsigned long long>(getDeviceMaxWorkGroupSize());
  ASSERT_MESSAGE(value <= (std::numeric_limits<unsigned long long>::max)(), "JNI cast overflow");
  return value;
}

jstring Java_opencl_executor_Executor_getDeviceName(JNIEnv * env, jclass)
{
  auto name = getDeviceName();
  return env->NewStringUTF(name.c_str());
}

jstring Java_opencl_executor_Executor_getDeviceType(JNIEnv * env, jclass)
{
  auto type = getDeviceType();
  return env->NewStringUTF(type.c_str());
}

jboolean Java_opencl_executor_Executor_supportsDouble(JNIEnv *, jclass)
{
  auto supports = supportsDouble();
  return static_cast<bool>(supports);
}

jboolean Java_opencl_executor_Executor_isLittleEndian(JNIEnv *, jclass)
{
  auto littleEndian = isLittleEndian();
  return static_cast<bool>(littleEndian);
}

void Java_opencl_executor_Executor_init__(JNIEnv *, jclass)
{
  initExecutor("ANY");
}

void Java_opencl_executor_Executor_shutdown(JNIEnv *, jclass)
{
  shutdownExecutor();
}

