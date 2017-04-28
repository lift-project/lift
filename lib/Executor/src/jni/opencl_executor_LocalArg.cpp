#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Weffc++"
#include "opencl_executor_LocalArg.h"
#pragma GCC diagnostic pop
#include "Handle.h"
#include "Executor.h"
#include "LocalArg.h"

jobject Java_opencl_executor_LocalArg_create(JNIEnv* env, jclass cls, jlong size)
{
  auto ptr = executor::LocalArg::create(size);
  auto methodID = env->GetMethodID(cls, "<init>", "(J)V"); 
  auto obj = env->NewObject(cls, methodID, ptr);
  return obj;
}

