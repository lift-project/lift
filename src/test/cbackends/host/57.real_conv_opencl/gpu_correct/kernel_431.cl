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
float multAndSumUp(float acc, float l, float r){
    {
        { return acc + (l * r); }; 
    }
}
kernel void KERNEL(const global float* restrict v__34, const global float* restrict v__35, const global float* restrict v__36, global float* v__43){
    #ifndef WORKGROUP_GUARD
    #define WORKGROUP_GUARD
    #endif
    WORKGROUP_GUARD
    {
        // Static local memory
        // Typed Value memory
        // Private Memory
        float v_v__38_1_55; 
        // map_seq
        //for (int v_i_29 = 0; (v_i_29 < 8); v_i_29 = (1 + v_i_29)){
        int v_i_29 = get_global_id(1); {
            // map_seq
            //for (int v_i_30 = 0; (v_i_30 < 6); v_i_30 = (1 + v_i_30)){
            int v_i_30 = get_global_id(0);{
                // map_seq
                //for (int v_i_31 = 0; (v_i_31 < 4); v_i_31 = (1 + v_i_31)){
                int v_i_31 = get_global_id(2); {
                    v_v__38_1_55 = id(v__36[v_i_31]); 
                    // reduce_seq
                    for (int v_i_32 = 0; (v_i_32 < 24); v_i_32 = (1 + v_i_32)){
                        v_v__38_1_55 = multAndSumUp(v_v__38_1_55, v__34[((v_i_32 % 3) + (27 * v_i_29) + (3 * v_i_30) + (3 * ((v_i_32 / 3) % 4)) + (27 * (v_i_32 / 12)))], v__35[(v_i_32 + (24 * v_i_31))]); 
                    }
                    // end reduce_seq
                    // map_seq
                    // unroll
                    v__43[(v_i_31 + (4 * v_i_30) + (24 * v_i_29))] = id(v_v__38_1_55); 
                    // end unroll
                    // end map_seq
                }
                // end map_seq
            }
            // end map_seq
        }
        // end map_seq
    }
}
