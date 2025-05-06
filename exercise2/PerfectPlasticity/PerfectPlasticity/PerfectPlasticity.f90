!===============================================================================
! Triaxial Test Simulation with Multiple Constitutive Models
! Author: Dr. Shreyas Giridharan
! Last Modified: April 28, 2025
! Lecture Date: May 6, 2025    
!
! This program simulates a triaxial test with three different constitutive models:
! 1. Linear Elastic
! 2. Drucker-Prager
! 3. Mohr-Coulomb
!
! The program outputs axial strain and deviatoric stress for each model.
!===============================================================================

program triaxial_test
    implicit none
    
    ! Declare variables
    integer, parameter :: dp = selected_real_kind(15, 307)  ! Double precision
    integer :: i, model_choice
    integer, parameter :: num_steps = 1000  ! Number of load steps
    real(dp) :: strain_increment, current_strain
    real(dp) :: axial_stress, radial_stress, dev_stress
    real(dp) :: p_stress, q_stress  ! Mean and deviatoric stress invariants
    real(dp) :: E, nu, G, K  ! Elastic parameters
    real(dp) :: friction_angle, cohesion, phi_rad
    real(dp) :: sin_phi, cos_phi, A_dp, B_dp  ! Drucker-Prager parameters
    real(dp) :: conf_pressure
    character(len=30) :: filename
    
    ! Initialize parameters
    conf_pressure = 200.0_dp    ! Confining pressure (kPa)
    E = 10000.0_dp              ! Young's modulus (kPa)
    nu = 0.3_dp                 ! Poisson's ratio
    friction_angle = 30.0_dp    ! Friction angle (degrees)
    cohesion = 0.0_dp          ! Cohesion (kPa)
    strain_increment = 0.0001_dp ! Axial strain increment per step
    
    ! Calculate elastic parameters
    G = E / (2.0_dp * (1.0_dp + nu))     ! Shear modulus
    K = E / (3.0_dp * (1.0_dp - 2.0_dp * nu)) ! Bulk modulus
    
    ! Convert friction angle to radians
    phi_rad = friction_angle * 3.14159265359_dp / 180.0_dp
    sin_phi = sin(phi_rad)
    cos_phi = cos(phi_rad)
    
    ! Calculate Drucker-Prager parameters 
    A_dp = 6.0_dp * cohesion * cos_phi / (sqrt(3.0_dp) * (3.0_dp - sin_phi))
    B_dp = 2.0_dp * sin_phi / (sqrt(3.0_dp) * (3.0_dp - sin_phi))
    
    ! Loop through all three models
    do model_choice = 1, 3
        ! Create output filename based on model
        select case(model_choice)
            case(1)
                filename = "elastic_results.dat"
            case(2)
                filename = "drucker_prager_results.dat"
            case(3)
                filename = "mohr_coulomb_results.dat"
        end select
        
        ! Open output file
        open(unit=10, file=filename, status='replace')
        write(10, '(A)') "# Axial_Strain, Deviatoric_Stress (kPa)"
        
        ! Initialize stresses
        axial_stress = conf_pressure
        radial_stress = conf_pressure
        dev_stress = 0.0_dp  ! No deviatoric stress initially
        
        ! Write initial values
        write(10, '(2F15.6)') 0.0_dp, dev_stress
        
        ! Simulation loop - apply axial strain incrementally
        do i = 1, num_steps
            current_strain = i * strain_increment
            
            ! Calculate stresses based on the chosen constitutive model
            select case(model_choice)
                case(1)
                    ! Linear Elastic Model
                    call elastic_model(current_strain, conf_pressure, E, nu, axial_stress)
                    radial_stress = conf_pressure
                    
                case(2)
                    ! Drucker-Prager Model (using A and B parameters)
                    call drucker_prager_model(current_strain, conf_pressure, E, nu, A_dp, B_dp, axial_stress)
                    radial_stress = conf_pressure
                    
                case(3)
                    ! Mohr-Coulomb Model (using direct MC criterion)
                    call mohr_coulomb_model(current_strain, conf_pressure, E, nu, cohesion, phi_rad, axial_stress)
                    radial_stress = conf_pressure
            end select
            
            ! Calculate deviatoric stress q = σ₁ - σ₃
            dev_stress = axial_stress - radial_stress
            
            ! Write results to file
            write(10, '(2F15.6)') current_strain, dev_stress
        end do
        
        ! Close the output file
        close(10)
        
        print *, "Simulation completed for model: ", trim(filename)
    end do
    
    print *, "All simulations completed successfully."
    
contains

    !----------------------------------------------------------------------
    ! Linear Elastic Model
    !----------------------------------------------------------------------
    subroutine elastic_model(axial_strain, conf_p, young, poisson, axial_stress)
        real(dp), intent(in) :: axial_strain, conf_p, young, poisson
        real(dp), intent(out) :: axial_stress
        
        ! Calculate axial stress using Hooke's law
        ! Under confined conditions where lateral stresses are constant:
        ! Δσ₁ = E*ε₁
        axial_stress = conf_p + young * axial_strain! E*ε₁ must be filled in
    end subroutine elastic_model
    
    !----------------------------------------------------------------------
    ! Drucker-Prager Model (Working in principal stress space)
    !----------------------------------------------------------------------
    subroutine drucker_prager_model(axial_strain, conf_p, young, poisson, A, B, axial_stress)
        real(dp), intent(in) :: axial_strain, conf_p, young, poisson, A, B
        real(dp), intent(out) :: axial_stress
        
        real(dp) :: elastic_axial, I1, J2, yield_func
        real(dp) :: sigma(3), yield_stress
        
        ! Calculate elastic trial stress
        call elastic_model(axial_strain, conf_p, young, poisson, elastic_axial)
        
        ! In triaxial test, principal stresses are:
        sigma(1) = elastic_axial  ! Axial stress (σ₁)
        sigma(2) = conf_p        ! Radial stress (σ₂)
        sigma(3) = conf_p          ! Radial stress (σ₃)
        
        ! Calculate stress invariants in principal stress space
        I1 = sigma(1) + sigma(2) + sigma(3)  ! First invariant (I₁ = σ₁ + σ₂ + σ₃)
        
        ! Calculate J2 (second invariant of deviatoric stress tensor)
        ! J₂ = 1/6 * [(σ₁-σ₂)² + (σ₂-σ₃)² + (σ₃-σ₁)²]
        J2 = (1.0_dp/6.0_dp) * ((sigma(1)-sigma(2))**2 + (sigma(2)-sigma(3))**2 + (sigma(3)-sigma(1))**2)! From the equation above, fill in the equation for second invariant
        
        ! Drucker-Prager yield criterion: √J₂ = A + B·I₁
        yield_func = SQRT(J2) - (A + B*I1)! Plastic flow happens when f=0. From the equation above, fill in the yield function's equation
        
        if (yield_func > 0.0_dp) then !think about when plastic correction is needed
            ! Plastic correction needed - enforce yield condition
            ! For triaxial test with constant confining pressure (σ₂ = σ₃ = conf_p),
            ! we can solve for σ₁ directly.
            
            ! From √J₂ = A + B·I₁
            ! For triaxial test: √J₂ = √((σ₁-σ₃)²/3)
            ! And I₁ = σ₁ + 2σ₃
            
            ! Therefore: (σ₁-σ₃)/√3 = A + B(σ₁+2σ₃)
            ! Solving for σ₁:
            ! σ₁/√3 - σ₃/√3 = A + B·σ₁ + 2B·σ₃
            ! σ₁/√3 - B·σ₁ = A + 2B·σ₃ + σ₃/√3
            ! σ₁(1/√3 - B) = A + σ₃(2B + 1/√3)
            ! σ₁ = (A + σ₃(2B + 1/√3)) / (1/√3 - B)
            
            yield_stress = (A + conf_p*(2.0_dp*B + 1.0_dp/sqrt(3.0_dp))) / (1.0_dp/sqrt(3.0_dp) - B)! from the derivation above, fill in the yield stress
            axial_stress = yield_stress
        else
            ! Still in elastic regime
            axial_stress = elastic_axial
        end if
    end subroutine drucker_prager_model
    
    !----------------------------------------------------------------------
    ! Mohr-Coulomb Model (Using direct MC criterion)
    !----------------------------------------------------------------------
    subroutine mohr_coulomb_model(axial_strain, conf_p, young, poisson, c, phi, axial_stress)
        real(dp), intent(in) :: axial_strain, conf_p, young, poisson, c, phi
        real(dp), intent(out) :: axial_stress
        
        real(dp) :: elastic_axial, sin_phi, cos_phi
        real(dp) :: sigma_1, sigma_3, MC_yield
        real(dp) :: tau_m, sigma_m
        
        ! Calculate elastic trial stress
        call elastic_model(axial_strain, conf_p, young, poisson, elastic_axial)
        
        sin_phi = sin(phi)
        cos_phi = cos(phi)
        
        ! In triaxial compression test:
        sigma_1 = elastic_axial  ! Major principal stress (axial)
        sigma_3 = conf_p         ! Minor principal stress (radial)
        
        ! Calculate sigma_m and tau_m as per the equations in Image 2
        sigma_m = (sigma_1 + sigma_3) / 2.0_dp
        tau_m = (sigma_1 - sigma_3) / 2.0_dp
        
        ! Check Mohr-Coulomb criterion 
        ! τ_m = σ_m sin(φ) + c cos(φ)
        MC_yield = sigma_m * sin_phi + C * cos_phi - tau_m! fill in the code from equatin above
        
        if (MC_yield < 0.0_dp) then ! think about what is different to the D-P criterion you just completed
            ! Plastic correction needed - enforce stress to be on yield surface
            ! Solve for sigma_1:
            ! τ_m = σ_m sin(φ) + c cos(φ)
            ! (σ₁ - σ₃)/2 = (σ₁ + σ₃)/2 * sin(φ) + c*cos(φ)
            ! σ₁ - σ₃ = (σ₁ + σ₃) * sin(φ) + 2*c*cos(φ)
            ! σ₁(1 - sin(φ)) = σ₃(1 + sin(φ)) + 2*c*cos(φ)
            ! σ₁ = σ₃ * (1 + sin(φ))/(1 - sin(φ)) + 2*c*cos(φ)/(1 - sin(φ))
            
            ! Calculate axial stress at yield:
            axial_stress = conf_p * (1.0_dp + sin_phi)/(1.0_dp - sin_phi) + &
                          2.0_dp * c * cos_phi / (1.0_dp - sin_phi)
! fill in using the equation above
        else
            ! Still in elastic regime
            axial_stress = elastic_axial
        end if
    end subroutine mohr_coulomb_model
    
end program triaxial_test