// NOTE: trying to print unprintable type: float
 // NOTE: trying to print unprintable type: float
#ifndef Tuple2_float_float_DEFINED
#define Tuple2_float_float_DEFINED
typedef struct __attribute__((aligned(4))){
    float _0;
    float _1;
} Tuple2_float_float;
#endif

float multAndSumUp(float acc, float l, float r){
    {
        { return acc + (l * r); }; 
    }
}
float id(float x){
    {
        { return x; }; 
    }
}
 void execute(const  float*  v__55, const  float*  v__56, const  float*  v__57,  float* v__69,  float* v__63, int v_K_3, int v_M_1, int v_N_0, int v_O_2){
    #ifndef WORKGROUP_GUARD
    #define WORKGROUP_GUARD
    #endif
    WORKGROUP_GUARD
    {
        // Static local memory
        // Typed Value memory
        float v__58; 
        float v__64; 
        // Private Memory
        // map_seq
        for (int v_i_48 = 0; (v_i_48 < v_M_1); v_i_48 = (1 + v_i_48)){
            // map_seq
            for (int v_i_49 = 0; (v_i_49 < v_N_0); v_i_49 = (1 + v_i_49)){
                float v_tmp_83 = 0.0f; 
                v__58 = v_tmp_83; 
                // reduce_seq
                for (int v_i_50 = 0; (v_i_50 < v_K_3); v_i_50 = (1 + v_i_50)){
                    v__58 = multAndSumUp(v__58, v__55[(v_i_50 + (v_K_3 * v_i_48))], v__56[(v_i_49 + (v_N_0 * v_i_50))]); 
                }
                // end reduce_seq
                // map_seq
                // iteration count is exactly 1, no loop emitted
                {
                    int v_i_51 = 0; 
                    v__63[(v_i_49 + (v_N_0 * v_i_48))] = id(v__58); 
                }
                // end map_seq
            }
            // end map_seq
            // map_seq
            for (int v_i_52 = 0; (v_i_52 < v_O_2); v_i_52 = (1 + v_i_52)){
                float v_tmp_84 = 0.0f; 
                v__64 = v_tmp_84; 
                // reduce_seq
                for (int v_i_53 = 0; (v_i_53 < v_N_0); v_i_53 = (1 + v_i_53)){
                    v__64 = multAndSumUp(v__64, v__57[(v_i_52 + (v_O_2 * v_i_53))], v__63[(v_i_53 + (v_N_0 * v_i_48))]); 
                }
                // end reduce_seq
                // map_seq
                // iteration count is exactly 1, no loop emitted
                {
                    int v_i_54 = 0; 
                    v__69[(v_i_52 + (v_O_2 * v_i_48))] = id(v__64); 
                }
                // end map_seq
            }
            // end map_seq
        }
        // end map_seq
    }
}