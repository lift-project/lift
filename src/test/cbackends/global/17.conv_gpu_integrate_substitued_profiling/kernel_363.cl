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
kernel void KERNEL(const global float* restrict v__105, const global float* restrict v__106, global float* v__114){
    #ifndef WORKGROUP_GUARD
    #define WORKGROUP_GUARD
    #endif
    WORKGROUP_GUARD
    {
        // Static local memory
        // Typed Value memory
        float v__108; 
        // Private Memory
        float v__109_0;
        // iteration count is exactly 1 or less, no loop emitted
        if ((get_group_id(2) < 1)){
            int v_wg_id_97 = get_group_id(2); 
            for (int v_wg_id_98 = get_group_id(1); (v_wg_id_98 < 2); v_wg_id_98 = (v_wg_id_98 + get_num_groups(1))){
                for (int v_wg_id_99 = get_group_id(0); (v_wg_id_99 < 3); v_wg_id_99 = (v_wg_id_99 + get_num_groups(0))){
                    // iteration count is exactly 1 or less, no loop emitted
                    if ((get_local_id(2) < 1)){
                        int v_l_id_100 = get_local_id(2); 
                        for (int v_l_id_101 = get_local_id(1); (v_l_id_101 < 36); v_l_id_101 = (v_l_id_101 + get_local_size(1))){
                            for (int v_l_id_102 = get_local_id(0); (v_l_id_102 < 18); v_l_id_102 = (v_l_id_102 + get_local_size(0))){
                                float v_tmp_159 = 0.0f; 
                                v__108 = v_tmp_159; 
                                v__109_0 = id(v__108); 
                                // reduce_seq
                                // iteration count is exactly 1, no loop emitted
                                {
                                    int v_i_103 = 0; 
                                    v__109_0 = vectorisableMultAndSumUp(v__109_0, v__106[((v_l_id_102 % 2) + (256 * v_wg_id_97) + (128 * v_wg_id_98) + (2 * ((v_l_id_102 / 2) % 3)) + (2 * (v_l_id_101 % 6)) + (16 * (v_l_id_101 / 6)) + (16 * (v_l_id_102 / 6)))], v__105[(v_l_id_102 + (18 * v_wg_id_99) + (18 * v_l_id_100))]); 
                                }
                                // end reduce_seq
                                // map_seq
                                // unroll
                                v__114[(v_l_id_102 + (3888 * v_wg_id_97) + (648 * v_wg_id_99) + (1944 * v_wg_id_98) + (18 * v_l_id_101) + (648 * v_l_id_100))] = id(v__109_0); 
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