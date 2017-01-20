#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Weffc++"
#include "opencl_executor_JNIHandle.h"
#pragma GCC diagnostic pop
#include "Handle.h"
#include "Executor.h"

void Java_opencl_executor_JNIHandle_dispose(JNIEnv* env, jobject obj)
{
  auto ptr = getHandle<executor::JNIHandle>(env, obj);
  clearHandle(env, obj);
  delete ptr;
}

