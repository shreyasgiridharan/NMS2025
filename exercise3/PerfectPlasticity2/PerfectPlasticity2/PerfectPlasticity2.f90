!===============================================================================
! Triaxial Test Simulation with Multiple Constitutive Models
! Author: Dr. Shreyas Giridharan
! Last Modified: May 14, 2025
! Lecture Date: May 6, 2025    
!
! This program simulates a triaxial test with three different constitutive models:
! 1. Linear Elastic
! 2. Matsuoka-Nakai
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
    real(dp) :: sin_phi, cos_phi  ! Friction parameters
    real(dp) :: k_MN  ! Matsuoka-Nakai parameter
    real(dp) :: conf_pressure
    character(len=30) :: filename
    
    ! Initialize parameters
    conf_pressure = 100.0_dp    ! Confining pressure (kPa)
    E = 10000.0_dp              ! Young's modulus (kPa)
    nu = 0.3_dp                 ! Poisson's ratio
    friction_angle = 40.0_dp    ! Friction angle (degrees)
    cohesion = 0.0_dp           ! Cohesion (kPa)
    strain_increment = 0.0001_dp ! Axial strain increment per step
    
    ! Calculate elastic parameters
    G = E / (2.0_dp * (1.0_dp + nu))     ! Shear modulus
    K = E / (3.0_dp * (1.0_dp - 2.0_dp * nu)) ! Bulk modulus
    
    ! Convert friction angle to radians
    phi_rad = friction_angle * 3.14159265359_dp / 180.0_dp
    sin_phi = sin(phi_rad)
    cos_phi = cos(phi_rad)
    
    ! Calculate Matsuoka-Nakai parameter
    ! The k_MN parameter is related to friction angle
    ! k_MN = 9 - 9*sin²(φ)/(1-sin²(φ))
    k_MN = 9.0_dp - 9.0_dp * sin_phi**2 / (1.0_dp - sin_phi**2)
    
    ! Loop through all three models
    do model_choice = 1, 3
        ! Create output filename based on model
        select case(model_choice)
            case(1)
                filename = "elastic_results.dat"
            case(2)
                filename = "matsuoka_nakai_results.dat"
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
                    ! Matsuoka-Nakai Model
                    call matsuoka_nakai_model(current_strain, conf_pressure, E, nu, k_MN, cohesion, axial_stress)
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
        axial_stress = conf_p + young * axial_strain
    end subroutine elastic_model
    
    !----------------------------------------------------------------------
    ! Matsuoka-Nakai Model
    !----------------------------------------------------------------------
    subroutine matsuoka_nakai_model(axial_strain, conf_p, young, poisson, k, c, axial_stress)
        real(dp), intent(in) :: axial_strain, conf_p, young, poisson, k, c
        real(dp), intent(out) :: axial_stress
        
        real(dp) :: elastic_axial
        real(dp) :: I1a, I2a, I3a  ! Stress invariants with cohesion shift
        real(dp) :: sigma(3)       ! Principal stresses
        real(dp) :: sigma_effective(3) ! Effective principal stresses with cohesion
        real(dp) :: yield_func
        real(dp) :: sin_phi, phi_rad, cos_phi, tan_phi
        real(dp) :: a_cohesion     ! Cohesion parameter: a = c'*cot(φ')
        
        ! Calculate elastic trial stress
        call elastic_model(axial_strain, conf_p, young, poisson, elastic_axial)
        
        ! In triaxial test, principal stresses are:
        sigma(1) = elastic_axial  ! Axial stress (σ₁)
        sigma(2) = conf_p         ! Radial stress (σ₂)
        sigma(3) = conf_p         ! Radial stress (σ₃)
        
        ! For the Matsuoka-Nakai parameter k, we can derive the relationship with friction angle phi
        ! k = 9*(1-sin²(φ))/(1-2*sin²(φ))
        ! This can be rewritten as: sin²(φ) = (9-k)/(9+k)
        
        ! Convert k to friction angle
        sin_phi = 
        phi_rad = asin(sin_phi)
        cos_phi = cos(phi_rad)
        
        ! Calculate cohesion parameter a = c'*cot(φ')
        if (abs(c) > 1.0e-10_dp .and. abs(sin_phi) > 1.0e-10_dp) then
            tan_phi = 
            a_cohesion = 
        else
            a_cohesion = 0.0_dp
        end if
        
        ! Calculate effective stresses including cohesion
        ! σᵃ = σ' + a
        sigma_effective(1) = sigma(1) + a_cohesion
        sigma_effective(2) = sigma(2) + a_cohesion
        sigma_effective(3) = sigma(3) + a_cohesion
        
        ! Calculate stress invariants with effective stresses
        ! I₁ᵃ = σ₁ᵃ + σ₂ᵃ + σ₃ᵃ
        I1a = 
        
        ! I₂ᵃ = -(σ₁ᵃσ₂ᵃ + σ₂ᵃσ₃ᵃ + σ₃ᵃσ₁ᵃ)
        I2a =
        
        ! I₃ᵃ = σ₁ᵃσ₂ᵃσ₃ᵃ
        I3a = 
        
        ! Matsuoka-Nakai yield function as per the given formula:
        ! f = -I₁ᵃI₂ᵃ - ((9-sin²φ)/cos²φ)*I₃ᵃ
        yield_func = 
        
        if (yield_func > 0.0_dp) then
            ! Plastic correction needed - enforce yield condition
            ! For triaxial test with constant confining pressure (σ₂ = σ₃ = conf_p)
            ! Under triaxial conditions, Matsuoka-Nakai yields the same results as Mohr-Coulomb
            
            ! Calculate the corrected axial stress using the Mohr-Coulomb equivalent solution
            axial_stress = conf_p * (1.0_dp + sin_phi)/(1.0_dp - sin_phi) + &
                          2.0_dp * c * cos_phi / (1.0_dp - sin_phi)
        else
            ! Still in elastic regime
            axial_stress = elastic_axial
        end if
    end subroutine matsuoka_nakai_model
    
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
        
        ! Calculate sigma_m and tau_m as per the Mohr-Coulomb equations
        sigma_m = (sigma_1 + sigma_3) / 2.0_dp
        tau_m = (sigma_1 - sigma_3) / 2.0_dp
        
        ! Check Mohr-Coulomb criterion
        ! τ_m = σ_m sin(φ) + c cos(φ)
        MC_yield = sigma_m * sin_phi + c * cos_phi - tau_m
        
        if (MC_yield < 0.0_dp) then
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
        else
            ! Still in elastic regime
            axial_stress = elastic_axial
        end if
    end subroutine mohr_coulomb_model
    
end program triaxial_test