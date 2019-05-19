
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef INTERP_UF_H
#define INTERP_UF_H
; 
float interp_uf(float acc, float x, float x1, float y1, float x2, float y2){
    if (x >= x1 && x <= x2) {auto a = (y1-y2)/(x1-x2); auto b = y1 - a*x1; return acc + a*x + b;} else return acc + 0.0f;; 
}

#endif
 ; 
void interp(float * v_initial_param_932_401, float * v_initial_param_933_402, float * v_initial_param_934_403, float * & v_user_func_954_405, int v_N_0, int v_M_1){
    // Allocate memory for output pointers
    v_user_func_954_405 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_399 = 0;(v_i_399 <= (-1 + v_N_0)); (++v_i_399)){
        // For each element reduced sequentially
        v_user_func_954_405[v_i_399] = 0.0f; 
        for (int v_i_400 = 0;(v_i_400 <= (-2 + v_M_1)); (++v_i_400)){
            v_user_func_954_405[v_i_399] = interp_uf(v_user_func_954_405[v_i_399], v_initial_param_932_401[v_i_399], v_initial_param_933_402[v_i_400], v_initial_param_934_403[v_i_400], v_initial_param_933_402[(1 + v_i_400)], v_initial_param_934_403[(1 + v_i_400)]); 
        }
    }
}
}; 