
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
void interp(float * v_initial_param_7904_3182, float * v_initial_param_7905_3183, float * v_initial_param_7906_3184, float * & v_user_func_7926_3186, int v_N_2763, int v_M_2764){
    // Allocate memory for output pointers
    v_user_func_7926_3186 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_3180 = 0;(v_i_3180 <= (-1 + v_N_2763)); (++v_i_3180)){
        {
            {
                
            }
            {
                
            }
        }
        // For each element reduced sequentially
        v_user_func_7926_3186[v_i_3180] = 0.0f; 
        for (int v_i_3181 = 0;(v_i_3181 <= (-2 + v_M_2764)); (++v_i_3181)){
            v_user_func_7926_3186[v_i_3180] = interp_uf(v_user_func_7926_3186[v_i_3180], v_initial_param_7904_3182[v_i_3180], v_initial_param_7905_3183[v_i_3181], v_initial_param_7906_3184[v_i_3181], v_initial_param_7905_3183[(1 + v_i_3181)], v_initial_param_7906_3184[(1 + v_i_3181)]); 
        }
    }
}
}; 