{
    float add(float x, float y){
        {
            return x + y;; 
        }
    }
    float id(float x){
        {
            { return x; }; 
        }
    }
    kernel void KERNEL(const global float* restrict v__7, global float* v__11, global float* v__10, int v_N_0){
        #ifndef WORKGROUP_GUARD
        #define WORKGROUP_GUARD
        #endif
        WORKGROUP_GUARD
        {
            // Static local memory
            // Typed Value memory
            float v__8; 
            // Private Memory
            for (int v_gl_id_6 = get_global_id(0); (v_gl_id_6 < v_N_0); v_gl_id_6 = (v_gl_id_6 + get_global_size(0))){
                float v_tmp_16 = 1.0f; 
                v__8 = v_tmp_16; 
                v__10[v_gl_id_6] = add(v__8, v__7[v_gl_id_6]); 
                v__11[v_gl_id_6] = id(v__10[v_gl_id_6]); 
            }
        }
    }
}