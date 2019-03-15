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
float vectorisableMultAndSumUp(float acc, float l, float r){
    {
        { return acc + (l * r); }; 
    }
}
kernel void KERNEL(const global float* restrict v__101, const global float* restrict v__102, global float* v__110){
    #ifndef WORKGROUP_GUARD
    #define WORKGROUP_GUARD
    #endif
    WORKGROUP_GUARD
    {
        // Static local memory
        // Typed Value memory
        float v__104; 
        // Private Memory
        float v__105_0;
        // iteration count is exactly 1 or less, no loop emitted
        if ((get_group_id(2) < 1)){
            int v_wg_id_93 = get_group_id(2); 
            for (int v_wg_id_94 = get_group_id(1); (v_wg_id_94 < 2); v_wg_id_94 = (v_wg_id_94 + get_num_groups(1))){
                for (int v_wg_id_95 = get_group_id(0); (v_wg_id_95 < 3); v_wg_id_95 = (v_wg_id_95 + get_num_groups(0))){
                    // iteration count is exactly 1 or less, no loop emitted
                    if ((get_local_id(2) < 1)){
                        int v_l_id_96 = get_local_id(2); 
                        for (int v_l_id_97 = get_local_id(1); (v_l_id_97 < 36); v_l_id_97 = (v_l_id_97 + get_local_size(1))){
                            for (int v_l_id_98 = get_local_id(0); (v_l_id_98 < 18); v_l_id_98 = (v_l_id_98 + get_local_size(0))){
                                float v_tmp_149 = 0.0f; 
                                v__104 = v_tmp_149; 
                                v__105_0 = id(v__104); 
                                // reduce_seq
                                // iteration count is exactly 1, no loop emitted
                                {
                                    int v_i_99 = 0; 
                                    v__105_0 = vectorisableMultAndSumUp(v__105_0, v__102[((v_l_id_98 % 2) + (2 * (v_l_id_97 % 6)) + (16 * (v_l_id_98 / 6)) + (256 * v_wg_id_93) + (128 * v_wg_id_94) + (16 * (v_l_id_97 / 6)) + (2 * ((v_l_id_98 / 2) % 3)))], v__101[(v_l_id_98 + (18 * v_wg_id_95))]); 
                                }
                                // end reduce_seq
                                // map_seq
                                // unroll
                                v__110[(v_l_id_98 + (18 * v_l_id_97) + (648 * v_wg_id_95) + (3888 * v_wg_id_93) + (1944 * v_wg_id_94))] = id(v__105_0); 
                                // end unroll
                                // end map_seq
                            }
                        }
                    }
                }
            }
        }
    }
}