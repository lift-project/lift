// Layer 0, tune point 2
// inputChannels -> 3, inputWidth -> 224, inputHeight -> 224,
// padFuncX -> 1, padFuncY -> 1,
// kernelWidth -> 3, kernelHeight -> 3,
// kernelStrideX -> 1, kernelStrideY -> 1,
// numKernels -> 64
// tiling_hook_ABTiling.cacheAaddressSpace -> Some(global), tiling_hook_ABTiling.cacheBaddressSpace -> Some(global), tiling_hook_ABTiling.orderOfCacheADims -> None, tiling_hook_ABTiling.orderOfCacheBDims -> Some(List(0, 1))
// tiling_hook_ABTiling.vectorizeTileALoad_Vectorize.vectorLen -> None
// tiling_hook_ABTiling.vectorizeTileBLoad_Vectorize.vectorLen -> None
// divideAndConquerHook_DivideAndConquer.conquerFun_0_Privatize.resultAddressSpace -> Some(global)
// caching_hook_ABTiling.cacheAaddressSpace -> None, caching_hook_ABTiling.cacheBaddressSpace -> Some(global), caching_hook_ABTiling.orderOfCacheADims -> None, caching_hook_ABTiling.orderOfCacheBDims -> Some(List(0, 1))
// caching_hook_ABTiling.vectorizeTileALoad_Vectorize.vectorLen -> None
// caching_hook_ABTiling.vectorizeTileBLoad_Vectorize.vectorLen -> None
// partReduceAccPrivatize_hook_Privatize.resultAddressSpace -> Some(global)
// partReduceVectorize_hook_Vectorize.vectorLen -> None
// tileWidth -> 32, tileHeight -> 224, tileDepth -> 1,
// kernelGroupSize -> 1,
// inputCacheSizeX -> 8, inputCacheSizeY -> 16,
// kernelCacheSize -> 1
// localSize0 -> 223
// localSize1 -> 1
// localSize2 -> 1
// nWrgs0 -> 64
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
kernel __attribute((reqd_work_group_size(223,1,1)))
void KERNEL(const global float* restrict v__91788, const global float* restrict v__91789, global float* v__91808, global float* v__91790, global float* v__91791, global float* v__91795, global float* v__91803){
    #ifndef WORKGROUP_GUARD
    #define WORKGROUP_GUARD
    #endif
    WORKGROUP_GUARD
    {
        // Static local memory
        // Typed Value memory
        float v__91746; 
        float v__91755; 
        // Private Memory
        float v__91796_0;
        float v__91800_0;
        float v__91804_0;
        // iteration count is exactly 1, no loop emitted
        {
            int v_wg_id_91761 = get_group_id(0); 
            // map_seq
            // iteration count is exactly 1, no loop emitted
            {
                int v_i_91762 = 0; 
                // map_seq
                for (int v_i_91763 = 0; (v_i_91763 < 7); v_i_91763 = (1 + v_i_91763)){
                    // iteration count is exactly 1 or less, no loop emitted
                    if ((get_local_id(0) < 1)){
                        int v_l_id_91764 = get_local_id(0); 
                        // map_seq
                        for (int v_i_91765 = 0; (v_i_91765 < 3); v_i_91765 = (1 + v_i_91765)){
                            // map_seq
                            for (int v_i_91766 = 0; (v_i_91766 < 9); v_i_91766 = (1 + v_i_91766)){
                                v__91790[(v_i_91766 + (27 * v_i_91763) + (27 * v_l_id_91764) + (189 * v_wg_id_91761) + (9 * v_i_91765))] = id(v__91789[(v_i_91766 + (27 * v_wg_id_91761) + (27 * v_l_id_91764) + (9 * v_i_91765))]); 
                            }
                            // end map_seq
                        }
                        // end map_seq
                    }
                    barrier(CLK_GLOBAL_MEM_FENCE);
                    // map_seq
                    for (int v_i_91767 = 0; (v_i_91767 < 224); v_i_91767 = (1 + v_i_91767)){
                        for (int v_l_id_91768 = get_local_id(0); (v_l_id_91768 < 32); v_l_id_91768 = (223 + v_l_id_91768)){
                            // map_seq
                            for (int v_i_91769 = 0; (v_i_91769 < 3); v_i_91769 = (1 + v_i_91769)){
                                // map_seq
                                for (int v_i_91770 = 0; (v_i_91770 < 9); v_i_91770 = (1 + v_i_91770)){
                                    v__91791[(v_i_91770 + (193536 * v_i_91763) + (9 * v_i_91769) + (1354752 * v_wg_id_91761) + (27 * v_l_id_91768) + (864 * v_i_91767))] = id(v__91788[(v_l_id_91768 + (v_i_91770 % 3) + (32 * v_i_91763) + (226 * v_i_91767) + (51076 * v_i_91769) + (226 * (v_i_91770 / 3)))]); 
                                }
                                // end map_seq
                            }
                            // end map_seq
                        }
                        barrier(CLK_GLOBAL_MEM_FENCE);
                    }
                    // end map_seq
                    // map_seq
                    for (int v_i_91771 = 0; (v_i_91771 < 3); v_i_91771 = (1 + v_i_91771)){
                        // iteration count is exactly 1 or less, no loop emitted
                        if ((get_local_id(0) < 1)){
                            int v_l_id_91772 = get_local_id(0); 
                            // map_seq
                            for (int v_i_91773 = 0; (v_i_91773 < 56); v_i_91773 = (1 + v_i_91773)){
                                // map_seq
                                for (int v_i_91774 = 0; (v_i_91774 < 16); v_i_91774 = (1 + v_i_91774)){
                                    // map_seq
                                    for (int v_i_91775 = 0; (v_i_91775 < 8); v_i_91775 = (1 + v_i_91775)){
                                        // map_seq
                                        for (int v_i_91776 = 0; (v_i_91776 < 3); v_i_91776 = (1 + v_i_91776)){
                                            // map_seq
                                            for (int v_i_91777 = 0; (v_i_91777 < 3); v_i_91777 = (1 + v_i_91777)){
                                                v__91795[(v_i_91777 + (1354752 * v_wg_id_91761) + (1152 * v_i_91773) + (193536 * v_i_91763) + (72 * v_i_91774) + (64512 * v_i_91771) + (64512 * v_l_id_91772) + (9 * v_i_91775) + (3 * v_i_91776))] = id(v__91791[(v_i_91777 + (864 * v_i_91774) + (193536 * v_i_91763) + (1354752 * v_wg_id_91761) + (9 * v_i_91771) + (27 * v_i_91775) + (3 * v_i_91776) + (216 * (v_i_91773 % 4)) + (13824 * (v_i_91773 / 4)))]); 
                                            }
                                            // end map_seq
                                        }
                                        // end map_seq
                                    }
                                    // end map_seq
                                }
                                // end map_seq
                                // map_seq
                                // iteration count is exactly 1, no loop emitted
                                {
                                    int v_i_91778 = 0; 
                                    // map_seq
                                    for (int v_i_91779 = 0; (v_i_91779 < 16); v_i_91779 = (1 + v_i_91779)){
                                        // map_seq
                                        for (int v_i_91780 = 0; (v_i_91780 < 8); v_i_91780 = (1 + v_i_91780)){
                                            float v_tmp_91997 = 0.0f; 
                                            v__91746 = v_tmp_91997; 
                                            v__91796_0 = id(v__91746); 
                                            // reduce_seq
                                            for (int v_i_91781 = 0; (v_i_91781 < 9); v_i_91781 = (1 + v_i_91781)){
                                                v__91800_0 = mult(v__91795[(v_i_91781 + (1152 * v_i_91773) + (64512 * v_i_91771) + (64512 * v_l_id_91772) + (1354752 * v_wg_id_91761) + (193536 * v_i_91763) + (9 * v_i_91780) + (72 * v_i_91779))], v__91790[(v_i_91781 + (9 * v_i_91771) + (27 * v_i_91763) + (27 * v_l_id_91772) + (189 * v_wg_id_91761))]); 
                                                v__91796_0 = add(v__91796_0, v__91800_0); 
                                            }
                                            // end reduce_seq
                                            // map_seq
                                            // unroll
                                            v__91803[(v_i_91780 + (21504 * v_i_91763) + (32 * v_i_91779) + (512 * (v_i_91773 / 4)) + (7168 * v_i_91771) + (7168 * v_l_id_91772) + (150528 * v_wg_id_91761) + (8 * (v_i_91773 % 4)))] = id(v__91796_0); 
                                            // end unroll
                                            // end map_seq
                                        }
                                        // end map_seq
                                    }
                                    // end map_seq
                                }
                                // end map_seq
                            }
                            // end map_seq
                        }
                    }
                    // end map_seq
                    // barrier(CLK_GLOBAL_MEM_FENCE); // MISSING BARRIER! (inserted manually)
                    // map_seq
                    // iteration count is exactly 1, no loop emitted
                    {
                        int v_i_91783 = 0; 
                        for (int v_l_id_91784 = get_local_id(0); (v_l_id_91784 < 224); v_l_id_91784 = (223 + v_l_id_91784)){
                            // map_seq
                            for (int v_i_91785 = 0; (v_i_91785 < 32); v_i_91785 = (1 + v_i_91785)){
                                float v_tmp_91998 = 0.0f; 
                                v__91755 = v_tmp_91998; 
                                v__91804_0 = id(v__91755); 
                                // reduce_seq
                                for (int v_i_91786 = 0; (v_i_91786 < 3); v_i_91786 = (1 + v_i_91786)){
                                    v__91804_0 = add(v__91804_0, v__91803[(v_i_91785 + (32 * v_l_id_91784) + (150528 * v_wg_id_91761) + (21504 * v_i_91763) + (7168 * v_i_91786))]); 
                                }
                                // end reduce_seq
                                // map_seq
                                // unroll
                                v__91808[(v_i_91785 + (50176 * v_wg_id_91761) + (32 * v_i_91763) + (224 * v_l_id_91784))] = id(v__91804_0); 
                                // end unroll
                                // end map_seq
                            }
                            // end map_seq
                        }
                    }
                    // end map_seq
                }
                // end map_seq
            }
            // end map_seq
        }
    }
}