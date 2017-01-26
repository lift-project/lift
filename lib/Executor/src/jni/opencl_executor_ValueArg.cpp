#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Weffc++"
#include "opencl_executor_ValueArg.h"
#pragma GCC diagnostic pop
#include "Handle.h"
#include "Executor.h"
#include "ValueArg.h"

template <typename T>
jobject helper(JNIEnv* env, jclass cls, T value)
{
  auto ptr = executor::ValueArg::create(&value, sizeof(value));
  auto methodID = env->GetMethodID(cls, "<init>", "(J)V"); 
  auto obj = env->NewObject(cls, methodID, ptr);
  return obj;
}

jobject Java_opencl_executor_ValueArg_create__F(JNIEnv* env, jclass cls,
                                                jfloat value)
{
  return helper(env, cls, value);
}

jobject Java_opencl_executor_ValueArg_create__I(JNIEnv* env, jclass cls,
                                                jint value)
{
  return helper(env, cls, value);
}

jobject Java_opencl_executor_ValueArg_create__D(JNIEnv* env, jclass cls,
                                                jdouble value)
{
  return helper(env, cls, value);
}

jobject Java_opencl_executor_ValueArg_create__Z(JNIEnv* env, jclass cls,
                                                jboolean value)
{
  return helper(env, cls, value);
}
