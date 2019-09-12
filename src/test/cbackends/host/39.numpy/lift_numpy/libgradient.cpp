
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef GRAD2_UF_H
#define GRAD2_UF_H
; 
float grad2_uf(float l, float r){
    { return (l - r)/2.0f; }; 
}

#endif
 ; 
void gradient(float * v_initial_param_7337_2980, float * & v_user_func_7343_2981, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7343_2981 = reinterpret_cast<float *>(malloc(((-2 + v_N_2763) * sizeof(float)))); 
    {
        
    }
    // For each element processed sequentially
    for (int v_i_2979 = 0;(v_i_2979 <= (-3 + v_N_2763)); (++v_i_2979)){
        v_user_func_7343_2981[v_i_2979] = grad2_uf(v_initial_param_7337_2980[(2 + v_i_2979)], v_initial_param_7337_2980[v_i_2979]); 
    }
}
}; 