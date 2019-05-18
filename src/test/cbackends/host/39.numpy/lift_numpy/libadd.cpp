
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
void add(float * v_initial_param_592_256, float * v_initial_param_593_257, float * & v_user_func_599_259, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_599_259 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_255 = 0;(v_i_255 <= (-1 + v_N_0)); (++v_i_255)){
        v_user_func_599_259[v_i_255] = add(v_initial_param_592_256[v_i_255], v_initial_param_593_257[v_i_255]); 
    }
}
}; 