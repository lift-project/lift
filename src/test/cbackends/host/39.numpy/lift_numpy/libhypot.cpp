
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef HYPOT_UF_H
#define HYPOT_UF_H
; 
float hypot_uf(float x, float y){
    { return sqrt((x*x)+(y*y)); }; 
}

#endif
 ; 
void hypot(float * v_initial_param_136_74, float * v_initial_param_137_75, float * & v_user_func_143_77, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_143_77 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_73 = 0;(v_i_73 <= (-1 + v_N_0)); (++v_i_73)){
        v_user_func_143_77[v_i_73] = hypot_uf(v_initial_param_136_74[v_i_73], v_initial_param_137_75[v_i_73]); 
    }
}
}; 