#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

void kendall(int* ranking, 
             int* reference, 
             int* candidates, 
             int* result)
{
  
  
  
  
 
  // printf("%d %d %d %d ", ranking[0], reference[0], candidates[0], result[0]);
  int total = 0; // number of swaps
  int ncandidates = *candidates; // Number of candidates
  int iter = 0, i, temp;
  
  while(iter < ncandidates) {
    for(i = 0; i < ncandidates-1; i ++) {
      if(reference[ranking[i]-1] > reference[ranking[i+1]-1]) {
        temp = ranking[i];
        ranking[i] = ranking[i+1];
        ranking[i+1] = temp;
        total++;
      }
    }
    iter++;
  }
  
  *result = total; 
}