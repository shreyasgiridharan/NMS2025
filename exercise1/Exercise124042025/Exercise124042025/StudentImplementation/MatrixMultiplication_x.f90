!==============================================================================
! Matrix Multiplication Program
! Author: Dr. Shreyas Giridharan
! Last Modified: April 22, 2025
! Numerical Modelling of Soils
! Lecture Date: April 24, 2025
!
! This program demonstrates matrix multiplication with performance
! considerations and error handling in Fortran.
! Some portions of the code are missing. Please fill it in. 
!==============================================================================

module matrix_operations
  implicit none
  private
  public :: matrix_multiply, matrix_allocate, matrix_deallocate
  public :: matrix_print, matrix_random_init, verify_multiplication

  ! Custom exception type for matrix operations
  type, public :: matrix_exception
    character(len=255) :: message
    integer :: error_code
  end type matrix_exception

contains

  !----------------------------------------------------------------------------
  ! Allocate memory for a matrix with error handling
  !----------------------------------------------------------------------------
  subroutine matrix_allocate(A, rows, cols, error)
    real(kind=8), allocatable, intent(out) :: A(:,:)
    integer, intent(in) :: rows, cols
    type(matrix_exception), intent(out), optional :: error
    
    integer :: alloc_stat
    character(len=255) :: error_msg

    ! Input validation
    if (rows <= 0 .or. cols <= 0) then
      if (present(error)) then
        error%message = "Invalid matrix dimensions"
        error%error_code = -1
      end if
      return
    end if
    
    ! Attempt allocation with error handling
    if (allocated(A)) deallocate(A, stat=alloc_stat)
    
    allocate(A(rows, cols), stat=alloc_stat, errmsg=error_msg)
    if (alloc_stat /= 0) then
      if (present(error)) then
        error%message = "Memory allocation failed: " // trim(error_msg)
        error%error_code = alloc_stat
      end if
    end if
  end subroutine matrix_allocate

  !----------------------------------------------------------------------------
  ! Deallocate matrix memory with error handling
  !----------------------------------------------------------------------------
  subroutine matrix_deallocate(A, error)
    real(kind=8), allocatable, intent(inout) :: A(:,:)
    type(matrix_exception), intent(out), optional :: error
    
    integer :: dealloc_stat
    character(len=255) :: error_msg

    if (allocated(A)) then
      deallocate(A, stat=dealloc_stat, errmsg=error_msg)
      if (dealloc_stat /= 0 .and. present(error)) then
        error%message = "Memory deallocation failed: " // trim(error_msg)
        error%error_code = dealloc_stat
      end if
    end if
  end subroutine matrix_deallocate

  !----------------------------------------------------------------------------
  ! Initialize matrix with random values
  !----------------------------------------------------------------------------
  subroutine matrix_random_init(A)
    real(kind=8), intent(out) :: A(:,:)
    integer :: i, j
    
    ! Seed the random number generator
    call random_seed()
    
    ! Fill matrix with random values between 0 and 1
    do j = 1, size(A, 2)
      do i = 1, size(A, 1)
        call random_number(A(i,j))
      end do
    end do
  end subroutine matrix_random_init

  !----------------------------------------------------------------------------
  ! Print matrix contents
  !----------------------------------------------------------------------------
  subroutine matrix_print(A, matrix_name)
    real(kind=8), intent(in) :: A(:,:)
    character(len=*), intent(in), optional :: matrix_name
    integer :: i, rows, cols
    
    rows = size(A, 1)
    cols = size(A, 2)
    
    if (present(matrix_name)) then
      print *, "Matrix ", trim(matrix_name), " (", rows, "×", cols, "):"
    else
      print *, "Matrix (", rows, "×", cols, "):"
    end if
    
    ! Print with formatting suitable for small matrices (for debugging)
    if (rows <= 10 .and. cols <= 10) then
      do i = 1, rows
        print '(*(F9.4,1X))', A(i,:)
      end do
    else
      print *, "(Matrix too large to display completely)"
      print *, "First 3x3 elements:"
      do i = 1, min(3, rows)
        print '(*(F9.4,1X))', A(i, 1:min(3, cols))
      end do
    end if
    print *
  end subroutine matrix_print

  !----------------------------------------------------------------------------
  ! Matrix multiplication implementation - STUDENT IMPLEMENTATION AREA
  !----------------------------------------------------------------------------
  subroutine matrix_multiply(A, B, C, error)
    real(kind=8), intent(in) :: A(:,:), B(:,:)
    real(kind=8), allocatable, intent(out) :: C(:,:)
    type(matrix_exception), intent(out), optional :: error
    
    integer :: m, n, p, i, j, k
    integer :: alloc_stat
    character(len=255) :: error_msg
    
    ! Get matrix dimensions
    m = size(A, 1)  ! Rows of A
    n = size(A, 2)  ! Columns of A
    p = size(B, 2)  ! Columns of B
    
    ! Check compatibility for multiplication
    if (n /= size(B, 1)) then
      if (present(error)) then
        error%message = "Incompatible matrix dimensions for multiplication"
        error%error_code = -2
      end if
      return
    end if
    
    ! Allocate result matrix
    call matrix_allocate(C, m, p, error)
    if (present(error)) then
      if (error%error_code /= 0) return
    end if
    
    !------------------------------------------------------------------------
    ! YOUR IMPLEMENTATION BEGINS
    !------------------------------------------------------------------------
    
    ! Initialize result matrix to zeros
 
    
    ! Perform matrix multiplication using triple loop - standard algorithm
 
    !------------------------------------------------------------------------
    ! YOUR IMPLEMENTATION ENDS
    !------------------------------------------------------------------------
    
  end subroutine matrix_multiply

  !----------------------------------------------------------------------------
  ! Verify matrix multiplication result for testing
  !----------------------------------------------------------------------------
  function verify_multiplication(A, B, C, tolerance) result(is_correct)
    real(kind=8), intent(in) :: A(:,:), B(:,:), C(:,:)
    real(kind=8), intent(in), optional :: tolerance
    logical :: is_correct
    
    real(kind=8) :: expected_value, actual_value, tol
    integer :: i, j, k, m, n, p
    
    ! Set default tolerance if not provided
    tol = 1.0e-10
    if (present(tolerance)) tol = tolerance
    
    m = size(A, 1)
    n = size(A, 2)
    p = size(B, 2)
    
    is_correct = .true.
    
    ! Check each element of C against manually calculated value
    outer_loop: do j = 1, p
      do i = 1, m
        expected_value = 0.0d0
        do k = 1, n
          expected_value = expected_value + A(i,k) * B(k,j)
        end do
        actual_value = C(i,j)
        
        if (abs(expected_value - actual_value) > tol) then
          is_correct = .false.
          exit outer_loop
        end if
      end do
    end do outer_loop
    
  end function verify_multiplication

end module matrix_operations

!==============================================================================
! Main program
!==============================================================================
program matrix_mult
  use matrix_operations
  implicit none
  
  real(kind=8), allocatable :: A(:,:), B(:,:), C(:,:)
  type(matrix_exception) :: error
  integer :: rows_A, cols_A, rows_B, cols_B
  real(kind=8) :: start_time, end_time
  logical :: verification

  ! Get matrix dimensions from user
  print *, "Enter dimensions for matrix A (rows cols):"
  read *, rows_A, cols_A
  
  print *, "Enter dimensions for matrix B (rows cols):"
  read *, rows_B, cols_B
  
  ! Check compatibility
  if (cols_A /= rows_B) then
    print *, "Error: Incompatible matrices. Columns of A must equal rows of B."
    stop
  end if
  
  ! Allocate and initialize matrices
  call matrix_allocate(A, rows_A, cols_A, error)
  if (error%error_code /= 0) then
    print *, "Error: ", trim(error%message)
    stop
  end if
  
  call matrix_allocate(B, rows_B, cols_B, error)
  if (error%error_code /= 0) then
    print *, "Error: ", trim(error%message)
    call matrix_deallocate(A)
    stop
  end if
  
  ! Initialize matrices with random values
  call matrix_random_init(A)
  call matrix_random_init(B)
  
  ! Print matrices (only if small enough)
  if (rows_A * cols_A <= 100) call matrix_print(A, "A")
  if (rows_B * cols_B <= 100) call matrix_print(B, "B")
  
  ! Perform matrix multiplication with timing
  call cpu_time(start_time)
  call matrix_multiply(A, B, C, error)
  call cpu_time(end_time)
  
  ! Check for errors during multiplication
  if (error%error_code /= 0) then
    print *, "Error during multiplication: ", trim(error%message)
    call matrix_deallocate(A)
    call matrix_deallocate(B)
    stop
  end if
  
  ! Print result matrix (only if small enough)
  if (rows_A * cols_B <= 100) call matrix_print(C, "C (result)")
  
  ! Verify result
  verification = verify_multiplication(A, B, C)
  
  ! Print timing and verification results
  print '(A,F10.6,A)', "Matrix multiplication took ", end_time - start_time, " seconds"
  print '(A,L1)', "Verification result: ", verification
  read (*,*)  ! Wait for user input before exiting
  
  ! Clean up
  call matrix_deallocate(A, error)
  call matrix_deallocate(B, error)
  call matrix_deallocate(C, error)

end program matrix_mult