#include <stdlib.h>
#include "matmul.h"

#define BLOCKSIZE 32

Matrix Allocate2ndMatrix(int height, int width, int init);

void matmul(float*, const float*, const float*, unsigned int, unsigned int, unsigned int);

////////////////////////////////////////////////////////////////////////////////
//! C = A * B
//! @param C          result matrix
//! @param A          matrix A 
//! @param B          matrix B
//! @param hA         height of matrix A
//! @param wB         width of matrix B
////////////////////////////////////////////////////////////////////////////////

void matmul(float* C, const float* A, const float* B, unsigned int hA, 
    unsigned int wA, unsigned int wB)
{
  // For each row of tiles in A
  for (unsigned int rowA = 0; rowA < hA; rowA+=BLOCKSIZE) {
    // For each row (column) of tiles in B
    for (unsigned int colB = 0; colB < wB; colB+=BLOCKSIZE) {
      // Advance the tile along the row/column in A/B
      for (unsigned int ktile = 0; ktile < hA; ktile+=BLOCKSIZE) {
        // For each row in the tile of C - unrolled by a factor of 2
        for (unsigned int i = rowA; i < rowA+BLOCKSIZE; ++i) {
          // For each column in the tile of C - unrolled by a factor of 2
          for (unsigned int j = colB; j < colB+BLOCKSIZE; ++j) {
            // Reading from C is necessary, because the tile is written
            // in stages
            double sum = C[i*wA +j];
            // do the dot product - 4 of them because we unroll two loops
            for (unsigned int k = ktile; k < ktile + BLOCKSIZE; ++k) {
              sum = sum + A[i*wA+k] * B[j*wB+k];
            } 
            C[i*wA+j] = sum;
          }
        }
      }
    }
  }
}

// Allocate a matrix of dimensions height*width
Matrix Allocate2ndMatrix(int height, int width)
{
  Matrix M;
  M.width = M.pitch = width;
  M.height = height;
  int size = M.width * M.height;
  M.elements = NULL;

  M.elements = (float*) malloc(size*sizeof(float));

  // this is a row-major allocation and initialization
  for(unsigned int row = 0; row < M.height; row++) {
    for(unsigned int col = 0; col < M.width; col++) {
      M.elements[col*M.height+row] = (rand() / (float)RAND_MAX);
    }
  }
  return M;
}	

