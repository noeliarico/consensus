#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

void scorix(int *profileOfRankings, 
            int *votes, 
            int *ncandidates, 
            int *nrankings,
            int *scorix) {
  
  //  numberOfVoters       ranking
  //                6 c ≻ b ≻ a ≻ d
  //                5 a ≻ d ≻ b ≻ c
  //                3 b ≻ a ≻ d ≻ c
  
  // votes
  // [6, 5, 3]
  
  // profile of rankings
  // [3, 2, 1, 4,   1, 3, 4, 2,   2, 1, 4, 3]
  
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
      scorix[(c*nc)+(profileOfRankings[c+(r*nc)]-1)] += votes[r];
    }
  }
   
} 