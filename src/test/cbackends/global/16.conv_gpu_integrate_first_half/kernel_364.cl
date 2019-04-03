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
kernel void KERNEL(const global float* restrict v__52, const global float* restrict v__53, global float* v__60){
    #ifndef WORKGROUP_GUARD
    #define WORKGROUP_GUARD
    #endif
    WORKGROUP_GUARD
    {
        // Static local memory
        // Typed Value memory
        // Private Memory
        float v__56_0;
        for (int v_wg_id_46 = get_group_id(1); (v_wg_id_46 < 2); v_wg_id_46 = (v_wg_id_46 + get_num_groups(1))){
            for (int v_wg_id_47 = get_group_id(0); (v_wg_id_47 < 3); v_wg_id_47 = (v_wg_id_47 + get_num_groups(0))){
                // iteration count is exactly 1 or less, no loop emitted
                if ((get_local_id(1) < 1)){
                    int v_l_id_48 = get_local_id(1); 
                    for (int v_l_id_49 = get_local_id(0); (v_l_id_49 < 36); v_l_id_49 = (v_l_id_49 + get_local_size(0))){
                        v__56_0 = id(v__52[v_wg_id_47]); 
                        // reduce_seq
                        for (int v_i_50 = 0; (v_i_50 < 18); v_i_50 = (1 + v_i_50)){
                            v__56_0 = add(v__56_0, v__53[(v_i_50 + (18 * v_l_id_49) + (1944 * v_wg_id_46) + (648 * v_wg_id_47))]); 
                        }
                        // end reduce_seq
                        // map_seq
                        // unroll
                        v__60[(v_l_id_49 + (36 * v_wg_id_47) + (108 * v_wg_id_46))] = id(v__56_0); 
                        // end unroll
                        // end map_seq
                    }
                }
            }
        }
    }
}