
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
#ifndef DIVMOD_UF_H
#define DIVMOD_UF_H
; 
Tuple2_float_float divmod_uf(float x, float y){
    return {int(x/y), x>=0? x - floor(x/y)*y : x - round(x/y)*y};; 
}

#endif
 ; 
void divmod(float * v_initial_param_743_334, float * v_initial_param_744_335, Tuple2_float_float * & v_user_func_750_337, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_750_337 = reinterpret_cast<Tuple2_float_float *>(malloc((v_N_0 * sizeof(Tuple2_float_float)))); 
    // For each element processed sequentially
    for (int v_i_333 = 0;(v_i_333 <= (-1 + v_N_0)); (++v_i_333)){
        v_user_func_750_337[v_i_333] = divmod_uf(v_initial_param_743_334[v_i_333], v_initial_param_744_335[v_i_333]); 
    }
}
}; 