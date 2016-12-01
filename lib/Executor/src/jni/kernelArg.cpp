#include "opencl_executor_KernelArg.h"
#include "handle.h"
#include "Executor.h"

void Java_opencl_executor_KernelArg_dispose(JNIEnv* env, jobject obj)
{
  auto ptr = getHandle<KernelArg>(env, obj);
  clearHandle(env, obj);
  delete ptr;
}

