
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
void trapz(float * v_initial_param_3444_578, float * v_initial_param_3445_579, float * & v_user_func_3448_582, int v_N_352){
    // Allocate memory for output pointers
    float * v_user_func_3472_581 = reinterpret_cast<float *>(malloc(((-1 + v_N_352) * sizeof(float))));
    v_user_func_3448_582 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    {
        {
            
        }
        {
            
        }
    }
    // For each element processed sequentially
    for (int v_i_577 = 0;(v_i_577 <= (-2 + v_N_352)); (++v_i_577)){
        v_user_func_3472_581[v_i_577] = trapz(v_initial_param_3444_578[v_i_577], v_initial_param_3444_578[(1 + v_i_577)], v_initial_param_3445_579[v_i_577], v_initial_param_3445_579[(1 + v_i_577)]); 
    }
    // For each element reduced sequentially
    v_user_func_3448_582[0] = 0.0f; 
    for (int v_i_576 = 0;(v_i_576 <= (-2 + v_N_352)); (++v_i_576)){
        v_user_func_3448_582[0] = add(v_user_func_3448_582[0], v_user_func_3472_581[v_i_576]); 
    }
}
}; 