#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Weffc++"
#include "opencl_executor_LocalArg.h"
#pragma GCC diagnostic pop
#include "handle.h"
#include "Executor.h"

jobject Java_opencl_executor_LocalArg_create(JNIEnv* env, jclass cls, jint size)
{
  auto ptr = LocalArg::create(size);
  auto methodID = env->GetMethodID(cls, "<init>", "(J)V"); 
  auto obj = env->NewObject(cls, methodID, ptr);
  return obj;
}

