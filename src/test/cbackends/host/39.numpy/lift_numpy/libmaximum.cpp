
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef MAXIMUM_UF_H
#define MAXIMUM_UF_H
; 
float maximum_uf(float x, float y){
    { return max(x,y); }; 
}

#endif
 ; 
void maximum(float * v_initial_param_3904_750, float * v_initial_param_3905_751, float * & v_user_func_3911_753, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3911_753 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_749 = 0;(v_i_749 <= (-1 + v_N_352)); (++v_i_749)){
        v_user_func_3911_753[v_i_749] = maximum_uf(v_initial_param_3904_750[v_i_749], v_initial_param_3905_751[v_i_749]); 
    }
}
}; 