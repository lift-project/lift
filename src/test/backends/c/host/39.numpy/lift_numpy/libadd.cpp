
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ADD_H
#define ADD_H
; 
float add(float l, float r){
    { return (l + r); }; 
}

#endif
 ; 
void add(float * v_initial_param_7575_3050, float * v_initial_param_7576_3051, float * & v_user_func_7582_3053, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7582_3053 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_3049 = 0;(v_i_3049 <= (-1 + v_N_2763)); (++v_i_3049)){
        v_user_func_7582_3053[v_i_3049] = add(v_initial_param_7575_3050[v_i_3049], v_initial_param_7576_3051[v_i_3049]); 
    }
}
}; 