#include<stdint.h>
#include<stdio.h>

typedef struct {uint64_t n; float* arr;} vector;
typedef struct {uint64_t n; vector* arr;} matrix; 
float vvmul(float s, vector* x, vector* y);
void vsmul(vector* res, float a, vector* y);
void mvmul(vector* res, matrix* a, vector* x); 
void transpose(matrix* res, matrix* a); 

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
  vsmul(&x, 3.0, &y); 
  for(int i=0; i<n; i++) printf("%f ", x.arr[i]); 
  printf("\n"); 
  float af[n][n]; af[0][1] = 1; 
  float bf[n][n];
  vector aarr[n]; 
  vector barr[n]; 
  for(int i=0; i<n; i++) aarr[i] = (vector){n, af[i]}; 
  for(int i=0; i<n; i++) barr[i] = (vector){n, bf[i]}; 
  matrix a = {n, aarr};
  matrix b = {n, barr};
  mvmul(&y, &a, &x); 
  printf("mv multiplication: ");
  for(int i=0; i<n; i++) printf("%f ", y.arr[i]); 
  printf("\n");
  printf("transpose(\n");
  transpose(&b, &a); 
  for(int i=0; i<n; i++) {
    for(int j=0; j<n; j++) printf("%f ", af[i][j]); 
    printf("\n"); 
  }
  printf(") = \n"); 
  for(int i=0; i<n; i++){
    for(int j=0; j<n; j++) printf("%f ", bf[i][j]); 
    printf("\n");
  }
  printf("\n"); 
}
