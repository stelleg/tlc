#include<stdint.h>
#include<stdio.h>

typedef struct {uint64_t n; float* arr;} vector;
float vvmul(float s, vector* x, vector* y);
float vsmul(vector* res, float a, vector* y);

int main(){
  int n = 10; 
  float yarr[n]; 
  float xarr[n]; 
  vector x = {n, xarr}; 
  vector y = {n, yarr}; 
  for(int i=0; i<n; i++) xarr[i] = 2;
  for(int i=0; i<n; i++) yarr[i] = 1;
  printf("Vector sum: %f\n", vvmul(0,&x,&y)); 
  printf("Scaled vector: ");
  vsmul (&x, 3.0, &y); 
  for(int i=0; i<n; i++) printf("%f ", x.arr[i]); 
  printf("\n"); 
}
