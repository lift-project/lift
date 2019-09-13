
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef TAN_UF_H
#define TAN_UF_H
; 
float tan_uf(float x){
    { return tan(x); }; 
}

#endif
 ; 
void tan(float * v_initial_param_7075_2857, float * & v_user_func_7077_2858, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7077_2858 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_2856 = 0;(v_i_2856 <= (-1 + v_N_2763)); (++v_i_2856)){
        v_user_func_7077_2858[v_i_2856] = tan_uf(v_initial_param_7075_2857[v_i_2856]); 
    }
}
}; 