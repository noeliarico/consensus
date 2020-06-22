#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <stdio.h>

void printarray(int arr[], int size)
{
  int i,j;
  for(i=0; i<size; i++)
  {
    Rprintf("%d",arr[i]);
    if(i!=size-1) {
      Rprintf(",");
    }
  }
  Rprintf("\n");
}

//function to swap the variables
void swap(int *a, int *b)
{
  int temp;
  temp = *a;
  *a = *b;
  *b = temp;
}

//permutation function
void permutationRec(int *arr, int start, int end)
{

  if(start==end)
  {
    printarray(arr, end+1);
    return;
  }
  int i;
  for(i=start;i<=end;i++)
  {
    //swapping numbers
    swap((arr+i), (arr+start));
    //fixing one first digit
    //and calling permutation on
    //the rest of the digits
    permutationRec(arr, start+1, end);
    swap((arr+i), (arr+start));
  }
}

void permutation(int *arr, int *pstart, int *pend) {
  int start = *pstart;
  int end = *pend;
  
  permutationRec(arr, 0, end);
}
