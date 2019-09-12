
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef LOGADDEXP2_UF_H
#define LOGADDEXP2_UF_H
; 
float logaddexp2_uf(float x1, float x2){
    { return log2(pow(2,x1) + pow(2,x2)); }; 
}

#endif
 ; 
void logaddexp2(float * v_initial_param_7498_3021, float * v_initial_param_7499_3022, float * & v_user_func_7505_3024, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7505_3024 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_3020 = 0;(v_i_3020 <= (-1 + v_N_2763)); (++v_i_3020)){
        v_user_func_7505_3024[v_i_3020] = logaddexp2_uf(v_initial_param_7498_3021[v_i_3020], v_initial_param_7499_3022[v_i_3020]); 
    }
}
}; 