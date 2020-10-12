#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

void scorix(int *profileOfRankings, 
            int *votes, 
            int *ncandidates, 
            int *nrankings,
            int *results) {
  
  int c, r, total;
  int nc = *ncandidates;
  int nr = *nrankings;
  int totpos = nc * nr;
   
  // printf("nc: %d \n", nc);
  // printf("nr: %d \n", nr);
  // printf("totpos: %d \n", totpos/2);
   
  for(c = 0; c < nc; c++) { // for each of the candidates
    for(r = 0; r < nr; r++) { // do that in each ranking
      //printf("The position of the candidate %d in the ranking %d is %d\n", c, r, profileOfRankings[c+(r*nc)]);
      // Add the position of the candidate c in the ranking r as the number
      // of voters of the ranking r
      results[(c*nc)+(profileOfRankings[c+(r*nc)]-1)] += votes[r];
    }
  }
}