
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef HYPOT_UF_H
#define HYPOT_UF_H
; 
float hypot_uf(float x, float y){
    { return sqrt((x*x)+(y*y)); }; 
}

#endif
 ; 
void hypot(float * v_initial_param_7103_2869, float * v_initial_param_7104_2870, float * & v_user_func_7110_2872, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7110_2872 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_2868 = 0;(v_i_2868 <= (-1 + v_N_2763)); (++v_i_2868)){
        v_user_func_7110_2872[v_i_2868] = hypot_uf(v_initial_param_7103_2869[v_i_2868], v_initial_param_7104_2870[v_i_2868]); 
    }
}
}; 