// NOTE: trying to print unprintable type: float
 // NOTE: trying to print unprintable type: float
#ifndef Tuple2_float_float_DEFINED
#define Tuple2_float_float_DEFINED
typedef struct __attribute__((aligned(4))){
    float _0;
    float _1;
} Tuple2_float_float;
#endif

 void execute1(const  float*  v__17, const  float*  v__18,  float* v__24, int v_K_3, int v_M_1, int v_N_0){
    #ifndef WORKGROUP_GUARD
    #define WORKGROUP_GUARD
    #endif
    WORKGROUP_GUARD
    {
        // Static local memory
        // Typed Value memory
        float v__19; 
        // Private Memory
        // map_seq
        for (int v_i_13 = 0; (v_i_13 < v_M_1); v_i_13 = (1 + v_i_13)){
            // map_seq
            for (int v_i_14 = 0; (v_i_14 < v_N_0); v_i_14 = (1 + v_i_14)){
                float v_tmp_32 = 0.0f; 
                v__19 = v_tmp_32; 
                // reduce_seq
                for (int v_i_15 = 0; (v_i_15 < v_K_3); v_i_15 = (1 + v_i_15)){
                    v__19 = multAndSumUp(v__19, v__17[(v_i_15 + (v_K_3 * v_i_13))], v__18[(v_i_14 + (v_N_0 * v_i_15))]); 
                }
                // end reduce_seq
                // map_seq
                // iteration count is exactly 1, no loop emitted
                {
                    int v_i_16 = 0; 
                    v__24[(v_i_14 + (v_N_0 * v_i_13))] = id(v__19); 
                }
                // end map_seq
            }
            // end map_seq
        }
        // end map_seq
    }
}