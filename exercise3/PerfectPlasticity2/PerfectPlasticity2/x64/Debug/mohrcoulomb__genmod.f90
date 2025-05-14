        !COMPILER-GENERATED INTERFACE MODULE: Thu Apr 24 13:58:20 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE MOHRCOULOMB__genmod
          INTERFACE 
            SUBROUTINE MOHRCOULOMB(E,NU,PHI,C,PSI,EPS,SIG,STATEVAR)
              REAL(KIND=8), INTENT(IN) :: E
              REAL(KIND=8), INTENT(IN) :: NU
              REAL(KIND=8), INTENT(IN) :: PHI
              REAL(KIND=8), INTENT(IN) :: C
              REAL(KIND=8), INTENT(IN) :: PSI
              REAL(KIND=8), INTENT(IN) :: EPS(3)
              REAL(KIND=8), INTENT(INOUT) :: SIG(4)
              REAL(KIND=8), INTENT(INOUT) :: STATEVAR(:)
            END SUBROUTINE MOHRCOULOMB
          END INTERFACE 
        END MODULE MOHRCOULOMB__genmod
