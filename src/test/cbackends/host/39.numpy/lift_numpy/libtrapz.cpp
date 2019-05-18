
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
void trapz(float * v_initial_param_405_195, float * v_initial_param_406_196, float * & v_user_func_409_199, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_433_198 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float))));
    v_user_func_409_199 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_194 = 0;(v_i_194 <= (-2 + v_N_0)); (++v_i_194)){
        v_user_func_433_198[v_i_194] = trapz(v_initial_param_405_195[v_i_194], v_initial_param_405_195[(1 + v_i_194)], v_initial_param_406_196[v_i_194], v_initial_param_406_196[(1 + v_i_194)]); 
    }
    // For each element reduced sequentially
    v_user_func_409_199[0] = 0.0f; 
    for (int v_i_193 = 0;(v_i_193 <= (-2 + v_N_0)); (++v_i_193)){
        v_user_func_409_199[0] = add(v_user_func_409_199[0], v_user_func_433_198[v_i_193]); 
    }
}
}; 