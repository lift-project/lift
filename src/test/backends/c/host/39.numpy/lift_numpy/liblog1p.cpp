
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef LOG1P_UF_H
#define LOG1P_UF_H
; 
float log1p_uf(float x){
    return log(1+x) ;; 
}

#endif
 ; 
void log1p(float * v_initial_param_7477_3013, float * & v_user_func_7479_3014, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7479_3014 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_3012 = 0;(v_i_3012 <= (-1 + v_N_2763)); (++v_i_3012)){
        v_user_func_7479_3014[v_i_3012] = log1p_uf(v_initial_param_7477_3013[v_i_3012]); 
    }
}
}; 