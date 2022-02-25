// Layer 10, tune point 7610
// inputChannels -> 512, inputWidth -> 14, inputHeight -> 14,
// padFuncX -> 1, padFuncY -> 1,
// kernelWidth -> 3, kernelHeight -> 3,
// kernelStrideX -> 1, kernelStrideY -> 1,
// numKernels -> 512
// tiling_hook_ABTiling.cacheAaddressSpace -> None, tiling_hook_ABTiling.cacheBaddressSpace -> None, tiling_hook_ABTiling.orderOfCacheADims -> None, tiling_hook_ABTiling.orderOfCacheBDims -> Some(List(1, 0))
// tiling_hook_ABTiling.vectorizeTileALoad_Vectorize.vectorLen -> None
// tiling_hook_ABTiling.vectorizeTileBLoad_Vectorize.vectorLen -> None
// DivideAndConquer.conquerFun_0_Privatize.resultAddressSpace -> Some(global)
// caching_hook_ABTiling.cacheAaddressSpace -> Some(private), caching_hook_ABTiling.cacheBaddressSpace -> Some(private), caching_hook_ABTiling.orderOfCacheADims -> None, caching_hook_ABTiling.orderOfCacheBDims -> Some(List(0, 1))
// caching_hook_ABTiling.vectorizeTileALoad_Vectorize.vectorLen -> None
// caching_hook_ABTiling.vectorizeTileBLoad_Vectorize.vectorLen -> None
// Privatize.resultAddressSpace -> Some(global)
// partReduceVectorize_hook_Vectorize.vectorLen -> None
// tileWidth -> 1, tileHeight -> 1, tileDepth -> 1,
// kernelGroupSize -> 1,
// inputCacheSizeX -> 1, inputCacheSizeY -> 1,
// kernelCacheSize -> 1
// localSize0 -> 1
// localSize1 -> 12
// localSize2 -> 1
// nWrgs0 -> 512
// nWrgs1 -> 1
// nWrgs2 -> 1
// NOTE: trying to print unprintable type: float
 // NOTE: trying to print unprintable type: float
#ifndef Tuple2_float_float_DEFINED
#define Tuple2_float_float_DEFINED
typedef struct __attribute__((aligned(4))){
    float _0;
    float _1;
} Tuple2_float_float;
#endif

float id(float x){
    {
        { return x; }; 
    }
}
float add(float x, float y){
    {
        { return x+y; }; 
    }
}
float mult(float l, float r){
    {
        { return l * r; }; 
    }
}
kernel void KERNEL(const global float* restrict v__622009478, const global float* restrict v__622009479, global float* v__622009497, global float* v__622009492){
    #ifndef WORKGROUP_GUARD
    #define WORKGROUP_GUARD
    #endif
    WORKGROUP_GUARD
    {
        // Static local memory
        // Typed Value memory
        float v__622009442; 
        float v__622009451; 
        // Private Memory
        float v__622009483_0;
        float v__622009483_1;
        float v__622009483_2;
        float v__622009483_3;
        float v__622009483_4;
        float v__622009483_5;
        float v__622009483_6;
        float v__622009483_7;
        float v__622009483_8;
        float v__622009484_0;
        float v__622009484_1;
        float v__622009484_2;
        float v__622009484_3;
        float v__622009484_4;
        float v__622009484_5;
        float v__622009484_6;
        float v__622009484_7;
        float v__622009484_8;
        float v__622009485_0;
        float v__622009489_0;
        float v__622009493_0;
        // map_seq
        for (int v_i_622009457 = 0; (v_i_622009457 < 512); v_i_622009457 = (1 + v_i_622009457)){
            for (int v_gl_id_622009458 = get_global_id(1); (v_gl_id_622009458 < 14); v_gl_id_622009458 = (12 + v_gl_id_622009458)){
                // map_seq
                for (int v_i_622009459 = 0; (v_i_622009459 < 14); v_i_622009459 = (1 + v_i_622009459)){
                    // iteration count is exactly 1, no loop emitted
                    {
                        int v_gl_id_622009460 = get_global_id(0); 
                        // map_seq
                        // iteration count is exactly 1, no loop emitted
                        {
                            int v_i_622009461 = 0; 
                            // map_seq
                            // iteration count is exactly 1, no loop emitted
                            {
                                int v_i_622009462 = 0; 
                                // map_seq
                                // iteration count is exactly 1, no loop emitted
                                {
                                    int v_i_622009463 = 0; 
                                    // map_seq
                                    // unroll
                                    // map_seq
                                    // unroll
                                    // map_seq
                                    // unroll
                                    v__622009483_0 = id(v__622009479[(9 * (v_gl_id_622009460 + (512 * v_i_622009457)))]); 
                                    v__622009483_1 = id(v__622009479[(1 + (9 * v_gl_id_622009460) + (4608 * v_i_622009457))]); 
                                    v__622009483_2 = id(v__622009479[(2 + (9 * v_gl_id_622009460) + (4608 * v_i_622009457))]); 
                                    v__622009483_3 = id(v__622009479[(3 * (1 + (3 * v_gl_id_622009460) + (1536 * v_i_622009457)))]); 
                                    v__622009483_4 = id(v__622009479[(4 + (9 * v_gl_id_622009460) + (4608 * v_i_622009457))]); 
                                    v__622009483_5 = id(v__622009479[(5 + (9 * v_gl_id_622009460) + (4608 * v_i_622009457))]); 
                                    v__622009483_6 = id(v__622009479[(3 * (2 + (3 * v_gl_id_622009460) + (1536 * v_i_622009457)))]); 
                                    v__622009483_7 = id(v__622009479[(7 + (9 * v_gl_id_622009460) + (4608 * v_i_622009457))]); 
                                    v__622009483_8 = id(v__622009479[(8 + (9 * v_gl_id_622009460) + (4608 * v_i_622009457))]); 
                                    // end unroll
                                    // end map_seq
                                    // end unroll
                                    // end map_seq
                                    // end unroll
                                    // end map_seq
                                    // map_seq
                                    // unroll
                                    v__622009484_0 = id(v__622009478[(v_gl_id_622009458 + (16 * v_i_622009459) + (256 * v_gl_id_622009460))]); 
                                    v__622009484_1 = id(v__622009478[(1 + v_gl_id_622009458 + (16 * v_i_622009459) + (256 * v_gl_id_622009460))]); 
                                    v__622009484_2 = id(v__622009478[(2 + v_gl_id_622009458 + (16 * v_i_622009459) + (256 * v_gl_id_622009460))]); 
                                    v__622009484_3 = id(v__622009478[(16 + v_gl_id_622009458 + (16 * v_i_622009459) + (256 * v_gl_id_622009460))]); 
                                    v__622009484_4 = id(v__622009478[(17 + v_gl_id_622009458 + (16 * v_i_622009459) + (256 * v_gl_id_622009460))]); 
                                    v__622009484_5 = id(v__622009478[(18 + v_gl_id_622009458 + (16 * v_i_622009459) + (256 * v_gl_id_622009460))]); 
                                    v__622009484_6 = id(v__622009478[(32 + v_gl_id_622009458 + (16 * v_i_622009459) + (256 * v_gl_id_622009460))]); 
                                    v__622009484_7 = id(v__622009478[(33 + v_gl_id_622009458 + (16 * v_i_622009459) + (256 * v_gl_id_622009460))]); 
                                    v__622009484_8 = id(v__622009478[(34 + v_gl_id_622009458 + (16 * v_i_622009459) + (256 * v_gl_id_622009460))]); 
                                    // end unroll
                                    // end map_seq
                                    // map_seq
                                    // unroll
                                    // map_seq
                                    // unroll
                                    // map_seq
                                    // unroll
                                    float v_tmp_622009640 = 0.0f; 
                                    v__622009442 = v_tmp_622009640; 
                                    v__622009485_0 = id(v__622009442); 
                                    // reduce_seq
                                    // unroll
                                    v__622009489_0 = mult(v__622009484_0, v__622009483_0); 
                                    v__622009485_0 = add(v__622009485_0, v__622009489_0); 
                                    v__622009489_0 = mult(v__622009484_1, v__622009483_1); 
                                    v__622009485_0 = add(v__622009485_0, v__622009489_0); 
                                    v__622009489_0 = mult(v__622009484_2, v__622009483_2); 
                                    v__622009485_0 = add(v__622009485_0, v__622009489_0); 
                                    v__622009489_0 = mult(v__622009484_3, v__622009483_3); 
                                    v__622009485_0 = add(v__622009485_0, v__622009489_0); 
                                    v__622009489_0 = mult(v__622009484_4, v__622009483_4); 
                                    v__622009485_0 = add(v__622009485_0, v__622009489_0); 
                                    v__622009489_0 = mult(v__622009484_5, v__622009483_5); 
                                    v__622009485_0 = add(v__622009485_0, v__622009489_0); 
                                    v__622009489_0 = mult(v__622009484_6, v__622009483_6); 
                                    v__622009485_0 = add(v__622009485_0, v__622009489_0); 
                                    v__622009489_0 = mult(v__622009484_7, v__622009483_7); 
                                    v__622009485_0 = add(v__622009485_0, v__622009489_0); 
                                    v__622009489_0 = mult(v__622009484_8, v__622009483_8); 
                                    v__622009485_0 = add(v__622009485_0, v__622009489_0); 
                                    // end unroll
                                    // end reduce_seq
                                    // map_seq
                                    // unroll
                                    v__622009492[(v_gl_id_622009460 + (512 * v_i_622009459) + (7168 * v_gl_id_622009458) + (100352 * v_i_622009457))] = id(v__622009485_0); 
                                    // end unroll
                                    // end map_seq
                                    // end unroll
                                    // end map_seq
                                    // end unroll
                                    // end map_seq
                                    // end unroll
                                    // end map_seq
                                }
                                // end map_seq
                            }
                            // end map_seq
                        }
                        // end map_seq
                    }
                    // barrier(CLK_GLOBAL_MEM_FENCE); // MISSING BARRIER! (inserted manually)
                    // iteration count is exactly 1 or less, no loop emitted
                    if ((get_global_id(0) < 1)){
                        int v_gl_id_622009473 = get_global_id(0); 
                        // map_seq
                        // iteration count is exactly 1, no loop emitted
                        {
                            int v_i_622009474 = 0; 
                            // map_seq
                            // iteration count is exactly 1, no loop emitted
                            {
                                int v_i_622009475 = 0; 
                                float v_tmp_622009641 = 0.0f; 
                                v__622009451 = v_tmp_622009641; 
                                v__622009493_0 = id(v__622009451); 
                                // reduce_seq
                                for (int v_i_622009476 = 0; (v_i_622009476 < 512); v_i_622009476 = (1 + v_i_622009476)){
                                    v__622009493_0 = add(v__622009493_0, v__622009492[(v_gl_id_622009473 + v_i_622009476 + (100352 * v_i_622009457) + (512 * v_i_622009459) + (7168 * v_gl_id_622009458))]); 
                                }
                                // end reduce_seq
                                // map_seq
                                // unroll
                                v__622009497[(v_gl_id_622009458 + (196 * v_i_622009457) + (14 * v_i_622009459) + (196 * v_gl_id_622009473))] = id(v__622009493_0); 
                                // end unroll
                                // end map_seq
                            }
                            // end map_seq
                        }
                        // end map_seq
                    }
                }
                // end map_seq
            }
        }
        // end map_seq
    }
}