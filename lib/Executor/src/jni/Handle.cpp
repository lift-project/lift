#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Weffc++"
#include <jni.h>
#pragma GCC diagnostic pop
#include "Handle.h"

jfieldID getHandleField(JNIEnv* env, jobject obj)
{
  auto c = env->GetObjectClass(obj);
  return env->GetFieldID(c, "nativeHandle", "J");
}

void clearHandle(JNIEnv* env, jobject obj)
{
  env->SetLongField(obj, getHandleField(env, obj), 0);
}

