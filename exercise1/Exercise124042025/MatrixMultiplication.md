# Matrix Multiplication Exercise

## Overview

This hands-on exercise focuses on implementing matrix multiplication in Fortran with considerations for performance and error handling. The exercise demonstrates several advanced Fortran features:

1. Derived types for error handling
2. Module-based program organization
3. Dynamic memory allocation with error checking
4. Computational performance considerations
5. Input validation and exception handling

## Program Structure

The program consists of two main components:

1. A `matrix_operations` module containing:
   - Memory allocation/deallocation routines
   - Matrix initialization and printing utilities
   - The core matrix multiplication algorithm
   - Verification functionality

2. A main program that:
   - Takes user input for matrix dimensions
   - Initializes matrices with random values
   - Times the multiplication operation
   - Verifies the result for correctness

## Student Task

Students should implement the core matrix multiplication algorithm in the subroutine `matrix_multiply`. The section marked for student implementation is clearly indicated:

```fortran
!------------------------------------------------------------------------
! STUDENT IMPLEMENTATION BEGINS
!------------------------------------------------------------------------

! ... student code goes here ...

!------------------------------------------------------------------------
! STUDENT IMPLEMENTATION ENDS
!------------------------------------------------------------------------
```

### Requirements

1. Implement the standard matrix multiplication algorithm:
   - For matrices A(m×n) and B(n×p), compute C(m×p)
   - The mathematical operation is: C(i,j) = Σ A(i,k) × B(k,j) for k=1..n

2. Consider performance aspects:
   - Think about memory access patterns and cache efficiency
   - Consider loop ordering for optimal performance
   - Initialize the result matrix to zeros before accumulation

3. Ensure correct handling of edge cases:
   - The implementation should work for any valid matrix dimensions
   - No need to re-check dimension compatibility (already checked)

## Performance Considerations

Students should be aware of these performance factors:

1. **Loop Ordering**: Fortran stores matrices in column-major order, so loops that traverse columns in the innermost loop may be more efficient.

2. **Memory Access Patterns**: Sequential memory access is faster than random access due to cache behavior.

3. **Compiler Optimization**: Modern Fortran compilers can often optimize simple matrix operations, but proper algorithm structure helps.

## Extensions (For Advanced Students)

1. Implement a blocked/tiled matrix multiplication algorithm for better cache efficiency

2. Add OpenMP parallelization to the matrix multiplication routine:
   ```fortran
   !$OMP PARALLEL DO PRIVATE(i,j,k)
   ```

3. Use Fortran's built-in matrix operations (with timing comparison):
   ```fortran
   C = MATMUL(A, B)
   ```

## Expected Output

For small matrices, the program will display:
- The input matrices A and B
- The resulting matrix C
- Execution time
- Verification result (TRUE if correct)

For large matrices, only timing information will be displayed.

## Evaluation Criteria

Students' implementations will be assessed based on:
1. Correctness (verified by the program)
2. Code clarity and documentation
3. Performance (execution time)
4. Proper handling of edge cases

## Learning Objectives

Through this exercise, students will:
- Implement a fundamental linear algebra operation in Fortran
- Gain experience with Fortran's array operations
- Consider performance implications of algorithm design
- Practice error handling in computational scientific code