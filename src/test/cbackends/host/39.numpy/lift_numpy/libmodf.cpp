
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
#ifndef MODF_UF_H
#define MODF_UF_H
; 
Tuple2_float_float modf_uf(float x){
    if(x>=0) return { x - floor(x), floor(x) }; else return  { x - round(x), round(x)} ;; 
}

#endif
 ; 
void modf(float * v_initial_param_706_310, Tuple2_float_float * & v_user_func_708_311, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_708_311 = reinterpret_cast<Tuple2_float_float *>(malloc((v_N_0 * sizeof(Tuple2_float_float)))); 
    // For each element processed sequentially
    for (int v_i_309 = 0;(v_i_309 <= (-1 + v_N_0)); (++v_i_309)){
        v_user_func_708_311[v_i_309] = modf_uf(v_initial_param_706_310[v_i_309]); 
    }
}
}; 