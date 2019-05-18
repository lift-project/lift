
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef TUPLE2_FLOAT_FLOAT_H
#define TUPLE2_FLOAT_FLOAT_H
; 
// NOTE: trying to print unprintable type: float
 // NOTE: trying to print unprintable type: float
typedef struct{
    float _0;
    float _1;
} Tuple2_float_float;


#endif
 ; 
#ifndef CONJ_UF_H
#define CONJ_UF_H
; 
Tuple2_float_float conj_uf(float x, float y){
    { return {x, (-1.0f)*y}; }; 
}

#endif
 ; 
void conj(Tuple2_float_float * v_initial_param_790_342, Tuple2_float_float * & v_user_func_796_343, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_796_343 = reinterpret_cast<Tuple2_float_float *>(malloc((v_N_0 * sizeof(Tuple2_float_float)))); 
    // For each element processed sequentially
    for (int v_i_341 = 0;(v_i_341 <= (-1 + v_N_0)); (++v_i_341)){
        v_user_func_796_343[v_i_341] = conj_uf(v_initial_param_790_342[v_i_341]._0, v_initial_param_790_342[v_i_341]._1); 
    }
}
}; 