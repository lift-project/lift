
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef DIV_UF_H
#define DIV_UF_H
; 
float div_uf(float x, float y){
    { return (x)/(y); }; 
}

#endif
 ; 
#ifndef ARCTAN_UF_H
#define ARCTAN_UF_H
; 
float arctan_uf(float x){
    { return atan(x); }; 
}

#endif
 ; 
void arctan2(float * v_initial_param_3173_464, float * v_initial_param_3174_465, float * & v_user_func_3176_468, int v_N_352){
    // Allocate memory for output pointers
    float * v_user_func_3184_467 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float))));
    v_user_func_3176_468 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_463 = 0;(v_i_463 <= (-1 + v_N_352)); (++v_i_463)){
        v_user_func_3184_467[v_i_463] = div_uf(v_initial_param_3173_464[v_i_463], v_initial_param_3174_465[v_i_463]); 
    }
    // For each element processed sequentially
    for (int v_i_462 = 0;(v_i_462 <= (-1 + v_N_352)); (++v_i_462)){
        v_user_func_3176_468[v_i_462] = arctan_uf(v_user_func_3184_467[v_i_462]); 
    }
}
}; 