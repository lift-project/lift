
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
void interp(float * v_initial_param_3960_771, float * v_initial_param_3961_772, float * v_initial_param_3962_773, float * & v_user_func_3982_775, int v_N_352, int v_M_353){
    // Allocate memory for output pointers
    v_user_func_3982_775 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_769 = 0;(v_i_769 <= (-1 + v_N_352)); (++v_i_769)){
        {
            {
                
            }
            {
                
            }
        }
        // For each element reduced sequentially
        v_user_func_3982_775[v_i_769] = 0.0f; 
        for (int v_i_770 = 0;(v_i_770 <= (-2 + v_M_353)); (++v_i_770)){
            v_user_func_3982_775[v_i_769] = interp_uf(v_user_func_3982_775[v_i_769], v_initial_param_3960_771[v_i_769], v_initial_param_3961_772[v_i_770], v_initial_param_3962_773[v_i_770], v_initial_param_3961_772[(1 + v_i_770)], v_initial_param_3962_773[(1 + v_i_770)]); 
        }
    }
}
}; 