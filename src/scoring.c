#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

/*
 * Function: pointsToRanking
 * ---------------------------------------------------------------
 * Takes a ranking 
 */
void pointsToRanking(int *ranking, int *nranking, double *points) {
  
  // number of different candidates in the ranking. If there are no
  // ties this is also the number of maximum candidates
  int n_of_candidates = *nranking;
  
  int pos = 1;
  int i = 0, j = 0;
  
  // The position of the candidate in the ranking is computed
  for(i = 0; i < n_of_candidates; i++) {
    pos = 1;
    
    
    for(j = 0; j < n_of_candidates; j++) {
      if(j != i) {
        if(points[j] > points[i]) {
          pos++;
        }
      }
    }
    ranking[i] = pos;
  }
}

void distanceToRanking(int *ranking, int *nranking, double *points) {
  // number of different positions in the rankings
  int n_of_candidates = *nranking;
  int pos = 1;
  int i = 0, j = 0;
  for(i = 0; i < n_of_candidates; i++) {
    pos = 1;
    for(j = 0; j < n_of_candidates; j++) {
      if(j != i) {
        if(points[j] > points[i]) {
          pos++;
        }
      }
    }
    ranking[i] = pos;
  }
}