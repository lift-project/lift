#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Weffc++"
#include "opencl_executor_GlobalArg.h"
#pragma GCC diagnostic pop
#include "Handle.h"
#include "Executor.h"
#include "GlobalArg.h"

jobject Java_opencl_executor_GlobalArg_createInput___3B(JNIEnv* env, jclass cls,
                                                        jbyteArray data)
{
  auto arrayPtr = env->GetByteArrayElements(data, nullptr);
  auto ptr = executor::GlobalArg::create(arrayPtr,
                               env->GetArrayLength(data) * sizeof(jbyte));
  env->ReleaseByteArrayElements(data, arrayPtr, JNI_ABORT);

  auto methodID = env->GetMethodID(cls, "<init>", "(J)V");
  auto obj = env->NewObject(cls, methodID, ptr);
  return obj;
}

jobject Java_opencl_executor_GlobalArg_createOutput(JNIEnv* env, jclass cls,
                                                    jlong size)
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
  auto dataPtr = reinterpret_cast<jfloat*>(vec.hostBuffer().data());
  return dataPtr[index];
}

jfloatArray Java_opencl_executor_GlobalArg_asFloatArray(JNIEnv* env,
                                                        jobject obj)
{
  auto ptr = getHandle<executor::GlobalArg>(env, obj);
  auto& vec = ptr->data();

  auto res = env->NewFloatArray(vec.size() / sizeof(jfloat));
  if (res == nullptr) return nullptr;

  env->SetFloatArrayRegion(res, 0, vec.size() / sizeof(jfloat),
                           reinterpret_cast<jfloat*>(vec.hostBuffer().data()));
  return res;
}

jintArray Java_opencl_executor_GlobalArg_asIntArray(JNIEnv* env, jobject obj)
{
  auto ptr = getHandle<executor::GlobalArg>(env, obj);
  auto& vec = ptr->data();

  auto res = env->NewIntArray(vec.size() / sizeof(jint));
  if (res == nullptr) return nullptr;

  env->SetIntArrayRegion(res, 0, vec.size() / sizeof(jint),
                         reinterpret_cast<jint*>(vec.hostBuffer().data()));
  return res;
}

jdoubleArray Java_opencl_executor_GlobalArg_asDoubleArray(JNIEnv* env, jobject obj)
{
  auto ptr = getHandle<executor::GlobalArg>(env, obj);
  auto& vec = ptr->data();

  auto res = env->NewDoubleArray(vec.size() / sizeof(jdouble));
  if (res == nullptr) return nullptr;

  env->SetDoubleArrayRegion(res, 0, vec.size() / sizeof(jdouble),
                         reinterpret_cast<jdouble*>(vec.hostBuffer().data()));
  return res;
}

jbooleanArray Java_opencl_executor_GlobalArg_asBooleanArray(JNIEnv* env, jobject obj)
{
  auto ptr = getHandle<executor::GlobalArg>(env, obj);
  auto& vec = ptr->data();

  auto res = env->NewBooleanArray(vec.size() / sizeof(jboolean));
  if (res == nullptr) return nullptr;

  env->SetBooleanArrayRegion(res, 0, vec.size() / sizeof(jboolean),
                         reinterpret_cast<jboolean*>(vec.hostBuffer().data()));
  return res;
}

jbyteArray Java_opencl_executor_GlobalArg_asByteArray(JNIEnv* env, jobject obj)
{
  auto ptr = getHandle<executor::GlobalArg>(env, obj);
  auto& vec = ptr->data();

  auto res = env->NewByteArray(vec.size() / sizeof(jbyte));
  if (res == nullptr) return nullptr;

  env->SetByteArrayRegion(res, 0, vec.size() / sizeof(jbyte),
                          reinterpret_cast<jbyte*>(vec.hostBuffer().data()));
  return res;
}
