#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Weffc++"
#include "opencl_executor_GlobalArg.h"
#pragma GCC diagnostic pop
#include "Handle.h"
#include "Executor.h"
#include "GlobalArg.h"

jobject Java_opencl_executor_GlobalArg_createInput___3F(JNIEnv* env, jclass cls,
                                                        jfloatArray data)
{
  auto arrayPtr = env->GetFloatArrayElements(data, nullptr);
  auto ptr = executor::GlobalArg::create(arrayPtr,
                               env->GetArrayLength(data) * sizeof(float));
  env->ReleaseFloatArrayElements(data, arrayPtr, JNI_ABORT);

  auto methodID = env->GetMethodID(cls, "<init>", "(J)V"); 
  auto obj = env->NewObject(cls, methodID, ptr);
  return obj;
}

jobject Java_opencl_executor_GlobalArg_createInput___3I(JNIEnv* env, jclass cls,
                                                        jintArray data)
{
  auto arrayPtr = env->GetIntArrayElements(data, nullptr);
  auto ptr = executor::GlobalArg::create(arrayPtr,
                               env->GetArrayLength(data) * sizeof(int));
  env->ReleaseIntArrayElements(data, arrayPtr, JNI_ABORT);

  auto methodID = env->GetMethodID(cls, "<init>", "(J)V");
  auto obj = env->NewObject(cls, methodID, ptr);
  return obj;
}

jobject Java_opencl_executor_GlobalArg_createInput___3D(JNIEnv* env, jclass cls,
                                                        jdoubleArray data)
{
  auto arrayPtr = env->GetDoubleArrayElements(data, nullptr);
  auto ptr = executor::GlobalArg::create(arrayPtr,
                               env->GetArrayLength(data) * sizeof(double));
  env->ReleaseDoubleArrayElements(data, arrayPtr, JNI_ABORT);

  auto methodID = env->GetMethodID(cls, "<init>", "(J)V");
  auto obj = env->NewObject(cls, methodID, ptr);
  return obj;
}

jobject Java_opencl_executor_GlobalArg_createOutput(JNIEnv* env, jclass cls,
                                                    jint size)
{
  auto ptr = executor::GlobalArg::create(size, true);
  auto methodID = env->GetMethodID(cls, "<init>", "(J)V"); 
  auto obj = env->NewObject(cls, methodID, ptr);
  return obj;
}

jfloat Java_opencl_executor_GlobalArg_at(JNIEnv* env, jobject obj, jint index)
{
  auto ptr = getHandle<executor::GlobalArg>(env, obj);
  auto& vec = ptr->data();
  vec.copyDataToHost();
  auto dataPtr = reinterpret_cast<float*>(vec.hostBuffer().data());
  return dataPtr[index];
}

jfloatArray Java_opencl_executor_GlobalArg_asFloatArray(JNIEnv* env,
                                                        jobject obj)
{
  auto ptr = getHandle<executor::GlobalArg>(env, obj);
  auto& vec = ptr->data();

  auto res = env->NewFloatArray(vec.size() / sizeof(float));
  if (res == nullptr) return nullptr;

  env->SetFloatArrayRegion(res, 0, vec.size() / sizeof(float),
                           reinterpret_cast<float*>(vec.hostBuffer().data()));
  return res;
}

jintArray Java_opencl_executor_GlobalArg_asIntArray(JNIEnv* env, jobject obj)
{
  auto ptr = getHandle<executor::GlobalArg>(env, obj);
  auto& vec = ptr->data();

  auto res = env->NewIntArray(vec.size() / sizeof(int));
  if (res == nullptr) return nullptr;

  env->SetIntArrayRegion(res, 0, vec.size() / sizeof(int),
                         reinterpret_cast<int*>(vec.hostBuffer().data()));
  return res;
}

jdoubleArray Java_opencl_executor_GlobalArg_asDoubleArray(JNIEnv* env, jobject obj)
{
  auto ptr = getHandle<executor::GlobalArg>(env, obj);
  auto& vec = ptr->data();

  auto res = env->NewDoubleArray(vec.size() / sizeof(double));
  if (res == nullptr) return nullptr;

  env->SetDoubleArrayRegion(res, 0, vec.size() / sizeof(double),
                         reinterpret_cast<double*>(vec.hostBuffer().data()));
  return res;
}
