
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <stdint.h>
#include <string.h>
#include <bits/stdc++.h>

using namespace std;

#include "util.hpp"

#define VEC_START_ADDR SHARED_SPACE_START_ADDR

void* trans_alloc(unsigned int size){
 static unsigned int current_pos = 0;
 void *memory_allocated = reinterpret_cast<void*>(VEC_START_ADDR + current_pos);
 current_pos += size;
 return memory_allocated;
 }

 char *syncSpmStartAddr = (char *)SYNC_SPM_START_ADDR;
 void barrier_wait(unsigned n) {
 // Create pointers to a global cv, mutex and a barrier.
 pthread_cond_t *condPtr = (pthread_cond_t *)(syncSpmStartAddr);
 pthread_barrier_t *barrierPtr = (pthread_barrier_t *)(condPtr + 1);
 pthread_mutex_t *mutexPtr = (pthread_mutex_t *)(barrierPtr + 1);
 // Start is used by LCP[0] with the condition variable to signal other cores to "go".
 bool *start = (bool *)(mutexPtr + 1);
 // TODO_SDH_10_31_18: creating a new barrier object for every instance: not a very clean/scalable approach.
 syncSpmStartAddr += sizeof(pthread_cond_t) + sizeof(pthread_barrier_t) + sizeof(pthread_mutex_t) + sizeof(uint32_t);
 if(LCP_TILE_ID() == 0) {
 // Initialize the barrier with the number of participants.
 pthread_barrier_init(barrierPtr, nullptr, n);
 // Signal "go" and broadcast to all cores waiting.
 STORE_BYTE(start, 1);
 // LCP_PRINTF("--> Signaling all cores to start -->\n");
 pthread_cond_broadcast(condPtr);
 } else {
 // Need to grab a lock before sleeping with a cv.
 pthread_mutex_lock(mutexPtr);
 while(*start == 0) {
 // Release the lock and sleep until signaled.
 pthread_cond_wait(condPtr, mutexPtr);
 }
 // Unlock and wait on barrier until GPEs are done.
 pthread_mutex_unlock(mutexPtr);
 }

 pthread_barrier_wait(barrierPtr);
 }

    ; 
void execute(float * v_initial_param_627106_215190, float * v_initial_param_627107_215191, float * & v_user_func_627123_215703, int v_K_215180, int v_M_215179, int v_N_215178){
    // Allocate memory for output pointers
    v_user_func_627123_215703 = reinterpret_cast<float *>(trans_alloc(((v_M_215179 * v_N_215178) * sizeof(float)))); 
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_627172 = 0;(gpe_loop_cvar_627172 < 4); (++gpe_loop_cvar_627172)){
        GPEQ_PUSH(gpe_loop_cvar_627172, reinterpret_cast<uint32_t>(v_initial_param_627106_215190)); 
        GPEQ_PUSH(gpe_loop_cvar_627172, reinterpret_cast<uint32_t>(v_initial_param_627107_215191)); 
        GPEQ_PUSH(gpe_loop_cvar_627172, reinterpret_cast<uint32_t>(v_user_func_627123_215703)); 
        GPEQ_PUSH(gpe_loop_cvar_627172, v_K_215180); 
        GPEQ_PUSH(gpe_loop_cvar_627172, v_M_215179); 
        GPEQ_PUSH(gpe_loop_cvar_627172, v_N_215178); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    // For each transmuter chip
    for (int v_i_215186 = 0;(v_i_215186 < ((v_M_215179)/(2))); (++v_i_215186)){
        for (int v_tile_batch_627173 = 0;(v_tile_batch_627173 <= 1); (++v_tile_batch_627173)){
            int v_virtual_tile_id_627174 = (LCP_TILE_ID() + (v_tile_batch_627173 * 2));
            if ((v_virtual_tile_id_627174 < 2))for (int v_gpe_batch_627175 = 0;(v_gpe_batch_627175 <= (v_N_215178 / 4)); (++v_gpe_batch_627175)){
                for (int v_gpe_627176 = 0;(v_gpe_627176 < 4); (++v_gpe_627176)){
                    GPEQ_PUSH(v_gpe_627176, (v_gpe_627176 + (4 * v_gpe_batch_627175))); 
                }
                {
                    
                }
                for (int v_gpe_627177 = 0;(v_gpe_627177 < 4); (++v_gpe_627177)){
                    
__asm__ __volatile__ (
"dmb\n\t"
);
    ; 
                    LCPQ_POP(v_gpe_627177); 
                }
            }
        }
        {
            
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}