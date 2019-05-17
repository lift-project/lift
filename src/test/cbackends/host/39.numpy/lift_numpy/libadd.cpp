
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
void add(float * v_initial_param_581_245, float * v_initial_param_582_246, float * & v_user_func_588_248, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_588_248 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_244 = 0;(v_i_244 <= (-1 + v_N_0)); (++v_i_244)){
        v_user_func_588_248[v_i_244] = add(v_initial_param_581_245[v_i_244], v_initial_param_582_246[v_i_244]); 
    }
}
}; 