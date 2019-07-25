
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
void add(float * v_initial_param_3631_639, float * v_initial_param_3632_640, float * & v_user_func_3638_642, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3638_642 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_638 = 0;(v_i_638 <= (-1 + v_N_352)); (++v_i_638)){
        v_user_func_3638_642[v_i_638] = add(v_initial_param_3631_639[v_i_638], v_initial_param_3632_640[v_i_638]); 
    }
}
}; 