
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ID_H
#define ID_H
; 
float id(float x){
    { return x; }; 
}

#endif
 ; 
void positive(float * v_initial_param_3652_647, float * & v_user_func_3654_648, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3654_648 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_646 = 0;(v_i_646 <= (-1 + v_N_352)); (++v_i_646)){
        v_user_func_3654_648[v_i_646] = id(v_initial_param_3652_647[v_i_646]); 
    }
}
}; 