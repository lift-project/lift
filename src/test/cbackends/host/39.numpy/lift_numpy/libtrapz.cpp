
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef TRAPZ_H
#define TRAPZ_H
; 
float trapz(float x1, float x2, float y1, float y2){
    { return (x2-x1)*(y2+y1)/2.0f; }; 
}

#endif
 ; 
#ifndef ADD_H
#define ADD_H
; 
float add(float l, float r){
    { return (l + r); }; 
}

#endif
 ; 
void trapz(float * v_initial_param_1663_350, float * v_initial_param_1664_351, float * & v_user_func_1667_354, int v_N_190){
    // Allocate memory for output pointers
    float * v_user_func_1691_353 = reinterpret_cast<float *>(malloc(((-1 + v_N_190) * sizeof(float))));
    v_user_func_1667_354 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_349 = 0;(v_i_349 <= (-2 + v_N_190)); (++v_i_349)){
        v_user_func_1691_353[v_i_349] = trapz(v_initial_param_1663_350[v_i_349], v_initial_param_1663_350[(1 + v_i_349)], v_initial_param_1664_351[v_i_349], v_initial_param_1664_351[(1 + v_i_349)]); 
    }
    // For each element reduced sequentially
    v_user_func_1667_354[0] = 0.0f; 
    for (int v_i_348 = 0;(v_i_348 <= (-2 + v_N_190)); (++v_i_348)){
        v_user_func_1667_354[0] = add(v_user_func_1667_354[0], v_user_func_1691_353[v_i_348]); 
    }
}
}; 