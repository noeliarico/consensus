#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

void votrix(int *profileOfRankings, 
            int *votes, 
            int *totalvotes,
            int *ncandidates, 
            int *nrankings,
            double *votrix) {

  int c1, c2, r;
  int nc = *ncandidates;
  int nr = *nrankings;
  int total_votes = *totalvotes;
  double total = 0.0;
  
  //printf("Number of candidates: %d; Number of rankings: %d \n", nc, nr);
  
  for(c1 = 0; c1 < nc/2; c1++) { // for each of the candidates
    for(c2 = c1+1; c2 < nc; c2++) { // evaluate the rest of the candidates
      //printf("Evaluating the candidate %d against the candidate %d \n", c1, c2);
      total = 0;
      for(r = 0; r < nr; r++) { // do that in each ranking
        // the rankings are stored in a vector
        if(profileOfRankings[c1+(r*nc)] > profileOfRankings[c2+(r*nc)]) {
          total += votes[r];
          // printf("c%d [%d] is better than c%d [%d] in ranking %d, total = %f \n", c1+1, profileOfRankings[c1+(r*nc)], c2+1, profileOfRankings[c2+(r*nc)], r+1, total);
        }
        else if(profileOfRankings[c1+(r*nc)] == profileOfRankings[c2+(r*nc)]){
          total += (0.5*votes[r]);
          // Rprintf("c%d [%d] is tied with c%d [%d] in ranking %d, total = %f \n", c1+1, profileOfRankings[c1+(r*nc)], c2+1, profileOfRankings[c2+(r*nc)], r+1, total);
        }
        else {
          // Rprintf("c%d [%d] is worst than c%d [%d] in ranking %d, total = %f \n", c1+1, profileOfRankings[c1+(r*nc)], c2+1, profileOfRankings[c2+(r*nc)], r+1, total);
        }
          // Rprintf("Is %d > %d?\n", profileOfRankings[c1+(r*nc)], profileOfRankings[c2+(r*nc)]);
      }
      // Update the row
      votrix[c2+(c1*nc)] = total;
      // Update the column
      votrix[c1+(c2*nc)] = total_votes - total;
    }
  }
  
  
} 