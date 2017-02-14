#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Weffc++"
#include "opencl_executor_Kernel.h"
#pragma GCC diagnostic pop
#include "Handle.h"
#include "Kernel.h"
#include "util/Logger.h"

jobject Java_opencl_executor_Kernel_create(JNIEnv* env, jclass cls, jstring jKernelSource, jstring jKernelName, jstring jBuildOptions)
{
  auto kernelSourcePtr = env->GetStringUTFChars(jKernelSource, nullptr);
  auto kernelNamePtr = env->GetStringUTFChars(jKernelName, nullptr);
  auto buildOptionsPtr = env->GetStringUTFChars(jBuildOptions, nullptr);

  auto ptr = executor::Kernel::create(kernelSourcePtr, kernelNamePtr, buildOptionsPtr);

  env->ReleaseStringUTFChars(jKernelSource, kernelSourcePtr);
  env->ReleaseStringUTFChars(jKernelName, kernelNamePtr);
  env->ReleaseStringUTFChars(jBuildOptions, buildOptionsPtr);

  auto methodID = env->GetMethodID(cls, "<init>", "(J)V"); 
  auto obj = env->NewObject(cls, methodID, ptr);
  
  return obj;
}

void Java_opencl_executor_Kernel_build(JNIEnv* env, jobject obj)
{
  try {

    auto ptr = getHandle<executor::Kernel>(env, obj);
    ptr->build();
    
  } catch(cl::Error err) {
    jclass jClass = env->FindClass("opencl/executor/Executor$ExecutorFailureException");
    if(!jClass) LOG_ERROR("[JNI ERROR] Cannot find the exception class");
    env->ThrowNew(jClass, (std::string("Executor failure: ") + err.what()).c_str());
  } catch(...) {
    jclass jClass = env->FindClass("opencl/executor/Executor$ExecutorFailureException");
    if(!jClass) LOG_ERROR("[JNI ERROR] Cannot find the exception class");
    env->ThrowNew(jClass, "Executor failure");
  }
}

