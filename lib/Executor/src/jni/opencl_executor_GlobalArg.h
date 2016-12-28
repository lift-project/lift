/* DO NOT EDIT THIS FILE - it is machine generated */
#include <jni.h>
/* Header for class opencl_executor_GlobalArg */

#ifndef _Included_opencl_executor_GlobalArg
#define _Included_opencl_executor_GlobalArg
#ifdef __cplusplus
extern "C" {
#endif
/*
 * Class:     opencl_executor_GlobalArg
 * Method:    createInput
 * Signature: ([F)Lopencl/executor/GlobalArg;
 */
JNIEXPORT jobject JNICALL Java_opencl_executor_GlobalArg_createInput___3F
  (JNIEnv *, jclass, jfloatArray);

/*
 * Class:     opencl_executor_GlobalArg
 * Method:    createInput
 * Signature: ([I)Lopencl/executor/GlobalArg;
 */
JNIEXPORT jobject JNICALL Java_opencl_executor_GlobalArg_createInput___3I
  (JNIEnv *, jclass, jintArray);

/*
 * Class:     opencl_executor_GlobalArg
 * Method:    createInput
 * Signature: ([D)Lopencl/executor/GlobalArg;
 */
JNIEXPORT jobject JNICALL Java_opencl_executor_GlobalArg_createInput___3D
  (JNIEnv *, jclass, jdoubleArray);

/*
 * Class:     opencl_executor_GlobalArg
 * Method:    createOutput
 * Signature: (I)Lopencl/executor/GlobalArg;
 */
JNIEXPORT jobject JNICALL Java_opencl_executor_GlobalArg_createOutput
  (JNIEnv *, jclass, jint);

/*
 * Class:     opencl_executor_GlobalArg
 * Method:    at
 * Signature: (I)F
 */
JNIEXPORT jfloat JNICALL Java_opencl_executor_GlobalArg_at
  (JNIEnv *, jobject, jint);

/*
 * Class:     opencl_executor_GlobalArg
 * Method:    asFloatArray
 * Signature: ()[F
 */
JNIEXPORT jfloatArray JNICALL Java_opencl_executor_GlobalArg_asFloatArray
  (JNIEnv *, jobject);

/*
 * Class:     opencl_executor_GlobalArg
 * Method:    asIntArray
 * Signature: ()[I
 */
JNIEXPORT jintArray JNICALL Java_opencl_executor_GlobalArg_asIntArray
  (JNIEnv *, jobject);

/*
 * Class:     opencl_executor_GlobalArg
 * Method:    asDoubleArray
 * Signature: ()[D
 */
JNIEXPORT jdoubleArray JNICALL Java_opencl_executor_GlobalArg_asDoubleArray
  (JNIEnv *, jobject);

#ifdef __cplusplus
}
#endif
#endif
