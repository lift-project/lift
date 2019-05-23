
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SIGN_UF_H
#define SIGN_UF_H
; 
float sign_uf(float x){
    { return x==0? 0: ( x< 0 ? -1 : 1 ); }; 
}

#endif
 ; 
void sign(float * v_initial_param_901_397, float * & v_user_func_903_398, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_903_398 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_396 = 0;(v_i_396 <= (-1 + v_N_0)); (++v_i_396)){
        v_user_func_903_398[v_i_396] = sign_uf(v_initial_param_901_397[v_i_396]); 
    }
}
}; 