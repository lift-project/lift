
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ARCTAN_UF_H
#define ARCTAN_UF_H
; 
float arctan_uf(float x){
    { return atan(x); }; 
}

#endif
 ; 
void arctan(float * v_initial_param_7096_2866, float * & v_user_func_7098_2867, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7098_2867 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_2865 = 0;(v_i_2865 <= (-1 + v_N_2763)); (++v_i_2865)){
        v_user_func_7098_2867[v_i_2865] = arctan_uf(v_initial_param_7096_2866[v_i_2865]); 
    }
}
}; 