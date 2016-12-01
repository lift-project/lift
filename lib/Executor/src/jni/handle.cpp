#include <jni.h>
#include "handle.h"

jfieldID getHandleField(JNIEnv* env, jobject obj)
{
  auto c = env->GetObjectClass(obj);
  return env->GetFieldID(c, "nativeHandle", "J");
}

void clearHandle(JNIEnv* env, jobject obj)
{
  env->SetLongField(obj, getHandleField(env, obj), 0);
}

