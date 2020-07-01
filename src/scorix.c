#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

void scorix(int *profileOfRankings, 
            int *votes, 
            int *ncandidates, 
            int *nrankings,
            int *scorix) {
  
  //  numberOfVoters       ranking
  //  1              6 c ≻ b ≻ a ≻ d
  //  2              5 a ≻ d ≻ b ≻ c
  //  3              3 b ≻ a ≻ d ≻ c
  
  // votes
  // [6, 5, 3]
  
  // profile of rankings
  // [3, 2, 1, 4,   1, 3, 4, 2,   2, 1, 4, 3]
  
  // results
  // [0, 0, 0, 0, number of times that a is better than a, b, c, d
  //  0, 0, 0, 0, number of times that b is better than a, b, c, d
  //  0, 0, 0, 0, number of times that c is better than a, b, c, d
  //  0, 0, 0, 0] number of times that d is better than a, b, c, d
  
  // results after
  // [0, 1, 2, 3, number of times that a is better than a, b, c, d
  //  1, 0, 2, 2, number of times that b is better than a, b, c, d
  //  1, 1, 0, 1, number of times that c is better than a, b, c, d
  //  0, 1, 2, 0] number of times that d is better than a, b, c, d
  
  printf("\nEstoy dentro de scorix\n");
  
  
  int c1, c2, r, total;
  int nc = *ncandidates;
  int nr = *nrankings;
  int totpos = nc * nr;
   
  printf("nc: %d \n", nc);
  printf("nr: %d \n", nr);
  printf("totpos: %d \n", totpos/2);
   
  for(c1 = 0; c1 < nc; c1++) {
    for(c2 = c1; c2 < nc; c2++) {
      for(r = 1; r <= nr; r++) {
        total = 0;
        if(profileOfRankings[c1*r] > profileOfRankings[c2*r]) {
          total++;
        }
        scorix[0] = total;
        scorix[0] = nr - total;
      }
    }
  }
  
   
} 