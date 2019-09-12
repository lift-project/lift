
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
#ifndef FREXP_UF_H
#define FREXP_UF_H
; 
Tuple2_float_float frexp_uf(float x){
    int exp; return {frexp(x,&exp), exp} ;; 
}

#endif
 ; 
void lift_frexp(float * v_initial_param_7554_3037, Tuple2_float_float * & v_user_func_7556_3038, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7556_3038 = reinterpret_cast<Tuple2_float_float *>(malloc((v_N_2763 * sizeof(Tuple2_float_float)))); 
    // For each element processed sequentially
    for (int v_i_3036 = 0;(v_i_3036 <= (-1 + v_N_2763)); (++v_i_3036)){
        v_user_func_7556_3038[v_i_3036] = frexp_uf(v_initial_param_7554_3037[v_i_3036]); 
    }
}
}; 