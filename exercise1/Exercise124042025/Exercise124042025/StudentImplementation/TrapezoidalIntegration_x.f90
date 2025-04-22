!==============================================================================
! Numerical Integration using Trapezoidal Rule
! Author: Dr. Shreyas Giridharan
! Last Modified: April 22, 2025
! Numerical Modelling of Soils
! Lecture Date: April 24, 2025
!
! This program demonstrates the implementation of the trapezoidal rule
! for numerical integration. The trapezoidal rule approximates the integral 
! by dividing the interval into subintervals and approximating each 
! subinterval with a trapezoid.
! Some portions of the code are missing. Please fill it in. 
!
! The function to be integrated is defined in the 'f' function.
!==============================================================================

program trapezoidal_integration
  implicit none
  
  !----------------------------------------------------------------------------
  ! VARIABLE DECLARATIONS
  !----------------------------------------------------------------------------
  ! Integration limits
  real(kind=8) :: a, b         ! Lower and upper bounds of integration
  
  ! Computational parameters
  integer :: n                 ! Number of subintervals
  integer :: i                 ! Loop counter
  real(kind=8) :: h            ! Step size (width of each subinterval)
  real(kind=8) :: x            ! Current x value
  real(kind=8) :: result       ! Final result of integration
  real(kind=8) :: exact        ! Exact value of the integral (if known)
  real(kind=8) :: error        ! Absolute error
  real(kind=8) :: x_left, x_right, f_left, f_right, area
  
  ! Output formatting
  character(len=50) :: fmt_header
  character(len=50) :: fmt_result
  
  !----------------------------------------------------------------------------
  ! INPUT PARAMETERS
  !----------------------------------------------------------------------------
  ! Define the function, integration limits, and number of intervals
  ! In a more sophisticated program, these could be read from user input
  ! or command-line arguments
  
  a = 0.0d0       ! Lower bound
  b = 1.0d0       ! Upper bound
  
  ! For the convergence study, we'll use different numbers of subintervals
  print *, "NUMERICAL INTEGRATION USING TRAPEZOIDAL RULE"
  print *, "--------------------------------------------"
  print *, "Integrating f(x) = x^2 + 2*sin(x) from x =", a, "to x =", b
  print *
  
  ! Format strings for output
  fmt_header = '(A10, A15, A15, A15)'
  fmt_result = '(I10, F15.8, F15.8, E15.8)'
  
  write(*, fmt_header) "Intervals", "Approximation", "Exact Value", "Error"
  write(*, '(A55)') "-------------------------------------------------------"
  
  ! Try different numbers of subintervals to observe convergence
  do n = 10, 1000, 100
    !--------------------------------------------------------------------------
    ! IMPLEMENTATION OF TRAPEZOIDAL RULE ALGORITHM
    !--------------------------------------------------------------------------
    
    ! Calculate step size
    h = (b - a) / real(n, kind=8)
    
    ! Initialize result with function values at endpoints (with weight 1/2)
    result =  ! Fill in the results. a and b are the endpoints, f(x) is the function. Assume 0.5 as weight!
    
    ! Add contribution from interior points
    do i = 1, n-1
      x = a + i*h
      result = result + f(x)
    end do
    
    ! Multiply by step size
    result = h * result
    
    !--------------------------------------------------------------------------
    ! CALCULATE EXACT VALUE AND ERROR (for this specific function)
    !--------------------------------------------------------------------------
    ! For f(x) = x^2 + 2*sin(x), the exact integral from 0 to 1 is:
    ! [x^3/3 - 2*cos(x)]_0^1 = 1/3 - 2*cos(1) + 2*cos(0) = 1/3 - 2*cos(1) + 2
    exact =  ! Exact value of the integral. Use the above equation to calculate the exact value.
    error =  ! Calculate the error. Use the absolute value of the difference between the result and the exact value.
    
    !--------------------------------------------------------------------------
    ! OUTPUT FORMATTING AND PRINTING
    !--------------------------------------------------------------------------
    ! Print results with consistent formatting
    write(*, fmt_result) n, result, exact, error
  end do
  
  ! More detailed study for a specific number of intervals
  n = 100
  print *
  print *, "DETAILED CALCULATION FOR", n, "INTERVALS"
  print *, "--------------------------------------------"
  
  h = (b - a) / real(n, kind=8)
  
  ! Show the contribution from each subinterval
  print *, "Interval | Left x  | Right x | f(Left) | f(Right) | Area"
  print *, "--------------------------------------------------------------"
  
  result = 0.0d0  ! Reset result
  
  do i = 0, n-1
    
    
    x_left = a + i*h
    x_right = a + (i+1)*h
    f_left = f(x_left)
    f_right = f(x_right)
    area = 0.5d0 * (f_left + f_right) * h
    
    write(*, '(I8, 2F10.5, 2F10.5, F10.8)') i+1, x_left, x_right, f_left, f_right, area
    
    ! Accumulate the result
    result = result + area
  end do
  
  print *, "--------------------------------------------------------------"
  write(*, '(A40, F15.8)') "Total Integral Approximation:", result
  write(*, '(A40, F15.8)') "Exact Value:", exact
  write(*, '(A40, E15.8)') "Absolute Error:", abs(result - exact)
  write(*, '(A40, E15.8)') "Relative Error (%):", abs(result - exact)/exact * 100.0d0
  read (*,*)  ! Wait for user input before closing the program
  
contains
  !----------------------------------------------------------------------------
  ! FUNCTION TO BE INTEGRATED
  !----------------------------------------------------------------------------
  function f(x) result(y)
    real(kind=8), intent(in) :: x
    real(kind=8) :: y
    
    ! Define the function to be integrated: f(x) = x^2 + 2*sin(x)
    y =  ! Function definition. Use the equation f(x) = x^2 + 2*sin(x)
  end function f
  
end program trapezoidal_integration