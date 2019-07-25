
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ROUND_UF_H
#define ROUND_UF_H
; 
float round_uf(float x){
    return ( ((int) ceil(x)) % 2 == 0 ? ceil(x) : ceil(x) -1) ;; 
}

#endif
 ; 
void round_(float * v_initial_param_3252_503, float * & v_user_func_3254_504, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3254_504 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_502 = 0;(v_i_502 <= (-1 + v_N_352)); (++v_i_502)){
        v_user_func_3254_504[v_i_502] = round_uf(v_initial_param_3252_503[v_i_502]); 
    }
}
}; 