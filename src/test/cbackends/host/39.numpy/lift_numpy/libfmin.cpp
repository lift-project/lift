
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef FMIN_UF_H
#define FMIN_UF_H
; 
float fmin_uf(float x, float y){
    { return min(x,y); }; 
}

#endif
 ; 
void fmin(float * v_initial_param_3946_765, float * v_initial_param_3947_766, float * & v_user_func_3953_768, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3953_768 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_764 = 0;(v_i_764 <= (-1 + v_N_352)); (++v_i_764)){
        v_user_func_3953_768[v_i_764] = fmin_uf(v_initial_param_3946_765[v_i_764], v_initial_param_3947_766[v_i_764]); 
    }
}
}; 