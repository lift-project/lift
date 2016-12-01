#include "opencl_executor_ValueArg.h"
#include "handle.h"
#include "Executor.h"

template <typename T>
jobject helper(JNIEnv* env, jclass cls, T value)
{
  auto ptr = ValueArg::create(&value, sizeof(value));
  auto methodID = env->GetMethodID(cls, "<init>", "(J)V"); 
  auto obj = env->NewObject(cls, methodID, ptr);
  return obj;
}

jobject Java_opencl_executor_ValueArg_create__F(JNIEnv* env, jclass cls,
                                                jfloat value)
{
  return helper<float>(env, cls, value);
}

jobject Java_opencl_executor_ValueArg_create__I(JNIEnv* env, jclass cls,
                                                jint value)
{
  return helper<int>(env, cls, value);
}

jobject Java_opencl_executor_ValueArg_create__D(JNIEnv* env, jclass cls,
                                                jdouble value)
{
  return helper<double>(env, cls, value);
}
