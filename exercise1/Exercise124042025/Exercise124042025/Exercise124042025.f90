!==============================================================================
! Fortran Language Basics Demonstration for Numerical Modelling of Soils
! Lecture Date: April 24, 2025   
! Author: Dr. Shreyas Giridharan
! Last Modified: April 22, 2025
!
! This program demonstrates fundamental Fortran language features:
! - Program structure and modules
! - Variable declarations and types
! - Array syntax and operations
! - Control structures
!==============================================================================

!==============================================================================
! MODULE SECTION
!==============================================================================
! Modules allow for encapsulation of data types, variables, and procedures
! They promote code organization and reusability
module utilities
  implicit none
  
  ! Module-level constants (parameters in Fortran terminology)
  real, parameter :: PI = 3.14159265358979323846
  integer, parameter :: MAX_ARRAY_SIZE = 100
  
  ! Derived type (custom data structure) definition
  type :: vector3d
    real :: x, y, z
  end type vector3d
  
  ! Interface for generic procedures
  interface norm
    module procedure norm_vector3d, norm_array
  end interface norm
  
  ! Private procedures and data (not accessible outside the module)
  private :: norm_array
  
contains
  ! Function to calculate the norm (magnitude) of a 3D vector
  function norm_vector3d(v) result(magnitude)
    type(vector3d), intent(in) :: v
    real :: magnitude
    
    magnitude = sqrt(v%x**2 + v%y**2 + v%z**2)
  end function norm_vector3d
  
  ! Function to calculate the norm of a numerical array
  function norm_array(arr) result(magnitude)
    real, dimension(:), intent(in) :: arr
    real :: magnitude
    
    magnitude = sqrt(sum(arr**2))
  end function norm_array
  
  ! Subroutine demonstrating array operations
  subroutine array_operations_demo(n)
    integer, intent(in) :: n
    real, dimension(n) :: a, b, c
    real, dimension(n, n) :: matrix
    integer :: i, j
    
    ! Initialize arrays with values
    a = [(real(i), i=1,n)]       ! Array constructor with implied do-loop
    b = [(sin(real(i)*PI/n), i=1,n)]
    
    ! Array operations - no explicit loops needed
    c = a + 2.0*b                ! Element-wise addition and multiplication
    c = c / maxval(c)            ! Normalize by maximum value
    
    ! Array sections and assignments
    c(1:n:2) = 0.0               ! Set even indices to zero (stride of 2)
    
    ! Initialize 2D array (matrix)
    do j = 1, n
      do i = 1, n
        matrix(i,j) = real(i+j)/real(n)
      end do
    end do
    
    ! Array intrinsic functions
    print *, "Sum of array a:", sum(a)
    print *, "Maximum value in matrix:", maxval(matrix)
    print *, "Location of minimum in array c:", minloc(c)
    
    ! Matrix operations
    matrix = matmul(transpose(matrix), matrix)  ! Matrix multiplication
    
  end subroutine array_operations_demo
  
end module utilities

!==============================================================================
! PROGRAM SECTION
!==============================================================================
program fortran_basics
  ! Use the previously defined module
  use utilities
  
  ! Disable implicit typing (good practice)
  implicit none
  
  !----------------------------------------------------------------------------
  ! VARIABLE DECLARATIONS AND TYPES
  !----------------------------------------------------------------------------
  ! Integer variables with different kinds
  integer :: i, count, status
  integer(kind=8) :: large_number  ! 8-byte integer for larger values
  
  ! Real (floating-point) variables
  real :: x, y, z
  real(kind=8) :: double_precision_value  ! Double precision floating point
  
  ! Complex numbers
  complex :: c1, c2
  
  ! Logical variables
  logical :: flag, is_converged
  
  ! Character variables
  character(len=80) :: message
  character(len=:), allocatable :: dynamic_string
  
  ! Arrays with different dimensions and attributes
  real, dimension(10) :: vector              ! 1D array of size 10
  real, dimension(3,3) :: matrix             ! 2D array (3x3 matrix)
  real, allocatable, dimension(:,:) :: grid  ! Allocatable 2D array
  
  ! Constants (parameters)
  real, parameter :: TOLERANCE = 1.0e-6
  
  ! Variables for custom type
  type(vector3d) :: position, velocity
  
  !----------------------------------------------------------------------------
  ! PROGRAM INITIALIZATION
  !----------------------------------------------------------------------------
  ! Simple assignments
  count = 0
  x = 2.5
  y = 3.7
  z = -1.2
  flag = .true.
  message = "Fortran Language Basics Demonstration"
  
  ! Complex number initialization and operation
  c1 = (1.0, 2.0)                ! Real part 1.0, imaginary part 2.0
  c2 = cmplx(3.0, -1.0)          ! Alternative initialization
  print *, "Complex multiplication:", c1 * c2
  
  ! Dynamic memory allocation
  allocate(character(len=20) :: dynamic_string)
  dynamic_string = "Dynamic allocation"
  
  allocate(grid(100, 100))
  grid = 0.0  ! Initialize all elements to zero
  
  ! Initialize custom type
  position = vector3d(1.0, 0.0, 0.0)
  velocity = vector3d(0.1, 0.2, 0.3)
  
  ! Print information about our vector
  print *, "Initial position:", position%x, position%y, position%z
  print *, "Position magnitude:", norm(position)
  
  !----------------------------------------------------------------------------
  ! CONTROL STRUCTURES DEMONSTRATION
  !----------------------------------------------------------------------------
  print *, "CONTROL STRUCTURES DEMONSTRATION"
  print *, "---------------------------------"
  
  ! IF-THEN-ELSE structures
  print *, "IF-THEN-ELSE Example:"
  if (x > y) then
    print *, "x is greater than y"
  else if (x < y) then
    print *, "x is less than y"
  else
    print *, "x is equal to y"
  end if
  
  ! CASE construct
  print *, "CASE Example:"
  select case(count)
    case (:-1)
      print *, "count is negative"
    case (0)
      print *, "count is zero"
    case (1:10)
      print *, "count is between 1 and 10"
    case default
      print *, "count is greater than 10"
  end select
  
  ! DO loop with explicit bounds
  print *, "DO Loop Example:"
  do i = 1, 5
    print *, "Iteration", i, ": i^2 =", i**2
  end do
  
  ! DO loop with a step value
  print *, "DO Loop with step value:"
  do i = 10, 0, -2
    print *, "Counting down:", i
  end do
  
  ! DO WHILE loop
  print *, "DO WHILE Example:"
  count = 1
  do while (count <= 5)
    print *, "While loop iteration:", count
    count = count + 1
  end do
  
  !----------------------------------------------------------------------------
  ! ARRAY OPERATIONS DEMONSTRATION
  !----------------------------------------------------------------------------
  print *, "ARRAY OPERATIONS DEMONSTRATION"
  print *, "-----------------------------"
  
  ! Initialize our vector
  vector = 0.0
  vector(1) = 1.0
  vector(2:4) = [2.0, 3.0, 4.0]  ! Array section assignment
  vector(5:10) = 5.0             ! Scalar broadcast to array section
  
  ! Array operations
  print *, "Original vector:", vector
  print *, "Vector + 1:", vector + 1.0
  print *, "Vector * 2:", vector * 2.0
  print *, "Sum of vector elements:", sum(vector)
  print *, "Where vector > 3:", pack(vector, vector > 3.0)
  
  ! Initialize matrix
  matrix = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0], [3, 3])
  
  print *, "Matrix:"
  do i = 1, 3
    print *, matrix(i, :)  ! Print each row
  end do
  
  print *, "Transpose of matrix:"
  ! Create a temporary variable for the transpose
  matrix = transpose(matrix)
  do i = 1, 3
    print *, matrix(i, :)  ! Print each row of the transposed matrix
  end do
  ! Transpose back to original for any further operations
  matrix = transpose(matrix)
  
  ! Call the array operations demo from the module
  call array_operations_demo(5)
  
  !----------------------------------------------------------------------------
  ! CLEANUP
  !----------------------------------------------------------------------------
  deallocate(dynamic_string)
  deallocate(grid)
  
  print *, "Program completed successfully."
  read (*,*)  ! Wait for user input before exiting
  
end program fortran_basics