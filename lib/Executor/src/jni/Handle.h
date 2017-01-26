#ifndef HANDLE_H_
#define HANDLE_H_

jfieldID getHandleField(JNIEnv* env, jobject obj);

template <typename T>
T* getHandle(JNIEnv* env, jobject obj)
{
  auto handle = env->GetLongField(obj, getHandleField(env, obj));
  return reinterpret_cast<T*>(handle);
}

template <typename T>
void setHandle(JNIEnv* env, jobject obj, T* t)
{
  auto handle = reinterpret_cast<jlong>(t);
  env->SetLongField(obj, getHandleField(env, obj), handle);
}

void clearHandle(JNIEnv* env, jobject obj);

#endif
