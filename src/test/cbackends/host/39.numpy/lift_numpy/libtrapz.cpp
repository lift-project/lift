
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
void trapz(float * v_initial_param_387_175, float * v_initial_param_388_176, float * & v_user_func_391_179, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_415_178 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float))));
    v_user_func_391_179 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_174 = 0;(v_i_174 <= (-2 + v_N_0)); (++v_i_174)){
        v_user_func_415_178[v_i_174] = trapz(v_initial_param_387_175[v_i_174], v_initial_param_387_175[(1 + v_i_174)], v_initial_param_388_176[v_i_174], v_initial_param_388_176[(1 + v_i_174)]); 
    }
    // For each element reduced sequentially
    v_user_func_391_179[0] = 0.0f; 
    for (int v_i_173 = 0;(v_i_173 <= (-2 + v_N_0)); (++v_i_173)){
        v_user_func_391_179[0] = add(v_user_func_391_179[0], v_user_func_415_178[v_i_173]); 
    }
}
}; 