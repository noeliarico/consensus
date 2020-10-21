#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>


void condorcet(int *bmatrix, 
               int *ncandidates, 
               int *results) {
  
  int nc = *ncandidates;
  unsigned int iter, i, j;
  int current = 0, tentative = 0, t1 = 0, t2 = 0;
  
  for(iter = 0; iter < nc; iter++) { 
    for(i = 0; i < nc; i++) { 
      if(i != iter) {
        for(j = 0; j < nc; j++) { 
          if(j != iter && j != i) {
      /*      t1 = votrix[(nc*iter)+i];
            t2 = votrix[(nc*j)+iter];
            current = votrix[(nc*j)+i];
            // printf("Iter %d: comparing [%d](%d, %d) and [%d](%d, %d) against [%d](%d, %d)\n",
            //         iter+1, t1, iter+1, i+1, t2, j+1, iter+1, current, i+1, j+1);
            tentative = MIN(t1, t2);
            if(current < tentative) {
              //  printf("UPDATE %d -> %d\n", current, tentative);
              votrix[(nc*j)+i] = tentative;
            } */
          } 
        } 
      }
    }
  }
} 