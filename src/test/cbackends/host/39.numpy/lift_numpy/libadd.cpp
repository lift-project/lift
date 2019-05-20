
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
void add(float * v_initial_param_635_289, float * v_initial_param_636_290, float * & v_user_func_642_292, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_642_292 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_288 = 0;(v_i_288 <= (-1 + v_N_0)); (++v_i_288)){
        v_user_func_642_292[v_i_288] = add(v_initial_param_635_289[v_i_288], v_initial_param_636_290[v_i_288]); 
    }
}
}; 