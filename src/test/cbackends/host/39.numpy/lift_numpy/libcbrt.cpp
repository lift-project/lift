
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef CBRT_UF_H
#define CBRT_UF_H
; 
float cbrt_uf(float x){
    { return cbrt(x); }; 
}

#endif
 ; 
void cbrt(float * v_initial_param_867_379, float * & v_user_func_869_380, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_869_380 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_378 = 0;(v_i_378 <= (-1 + v_N_0)); (++v_i_378)){
        v_user_func_869_380[v_i_378] = cbrt_uf(v_initial_param_867_379[v_i_378]); 
    }
}
}; 