PROGRAM SCHROD
    ! has to pass E R0 RN and N as parameters like that
    ! ./out.a E N R0 RN out_name
    ! 
    ! modified version that will loop
    ! on the e with hard coded boundaries
    ! 
    ! got rid of the saving part
    ! precision tolerance is 0 to get rid of the stop check
    ! increment is now step and not DE
    ! 
    !    
       IMPLICIT NONE
       INTEGER(KIND=4) :: N = 50
       INTEGER(KIND=4):: NCMAX, IFLAG, M, JLEV, NCOUNT, NODE, I
       REAL(KIND=8), dimension(:), allocatable:: V, F, RR
       REAL(KIND=8):: R0, RN, A, B, EPS, TOL, FINIT, ENERG, H, HC, E, DE
       DATA R0/0.1D0/, RN/2.6D0/, A/1.3924/, B/1.484e-3/, EPS/1.0D-04/, TOL/0.0/, &
          FINIT/0.D0/, ENERG/-0.6D0/, NCMAX/1000/, IFLAG/1/, M/11/, JLEV/0/
       character(32) :: arg ! commandline argument holder

       real :: step = -0.00038

       
       !commandline stuff with arguments for automation
       call get_command_argument(1, arg)
       read(arg, *) ENERG
       
       call get_command_argument(2, arg)
       read(arg, *) N
       
       call get_command_argument(3, arg)
       read(arg, *) R0
       
       call get_command_argument(4, arg)
       read(arg, *) RN

    
       allocate(V(0:N), F(0:N), RR(0:N))
       
       
       
       
       H = (RN - R0)/DFLOAT(N)  ! calculation of the step:
       HC = H*H/B ! expression h2/b
    
       ! calculation of the vector V(0:N) containing the potential
       CALL POT(N, R0, H, A, B, JLEV, RR, V)
       ! putting the trial energy value:
       E = ENERG ! third step, initial guess value for e
       NCOUNT = 0
    !    PRINT *, "INITIAL GUESS ", E
    !    WRITE (*, *) "No_ITER  ENERG           M         DE        NODES"
    3  CONTINUE
       ! propagation; NCOUNT giving the number of iterations:
       CALL SHOOT(N, A, H, HC, FINIT, EPS, E, V, F, NODE, DE, M, IFLAG) ! fourth and fifth step, propagation
       NCOUNT = NCOUNT + 1
       WRITE (*, 200) NCOUNT, E, M, DE, NODE
    200 FORMAT(I4, 5X, E15.7, I4, E15.7, I4)
       IF (NCOUNT .GT. NCMAX) GOTO 999 ! divergence test?
       E = E + step 
       IF (ABS(DE/E) .GT. TOL) GOTO 3 ! check for tolerance and escape
       ! writing the wave function:

    
       STOP
    999 WRITE (*, *) "No convergence reached with", NCMAX, " iterations"
    END
    
    SUBROUTINE POT(N, R0, H, A, B, JLEV, RR, V)
       IMPLICIT NONE
       INTEGER(KIND=4):: N, JLEV, I
       REAL(KIND=8), DIMENSION(0:N):: V, RR
       REAL(KIND=8):: R0, H, A, B
       DO I = 0, N
          RR(I) = R0 + I*H ! first step described
          V(I) = DEXP(-2.D0*A*(RR(I) - 1.D0)) - 2.D0*DEXP(-A*(RR(I) - 1.D0)) ! second step described
       END DO
       RETURN
    END
    
    SUBROUTINE SHOOT(N, A, H, HC, FINIT, EPS, E, V, F, NODE, DE, M, IFLAG)
       IMPLICIT NONE
       INTEGER(KIND=4):: N, NODE, M, IFLAG, ISTEP, IINIT, IFIN, I
       REAL(KIND=8), DIMENSION(0:N):: V, F
       REAL(KIND=8):: A, B, H, HC, FINIT, EPS, E, DE, F0, F1, COEFF, AN, FSM, FM
       ISTEP = 1   ! propagation do the right
       IINIT = 1
       IFIN = N - 1
       IF (IFLAG .EQ. 0) IFIN = M
       F0 = FINIT
       F1 = EPS
       CALL PROPAG(N, HC, ISTEP, IINIT, IFIN, IFLAG, F0, F1, E, V, F, M) ! first propagation, step 4
       FM = F(M)
       ISTEP = -1 ! propagation to the left
       IINIT = N - 1
       IFIN = M + 1
       F0 = FINIT
       F1 = EPS
       CALL PROPAG(N, HC, ISTEP, IINIT, IFIN, IFLAG, F0, F1, E, V, F, M) ! step 5 propagation 2
       COEFF = F(M)/FM ! step 6 normalisation
       DO I = 0, M - 1
          F(I) = F(I)*COEFF
       END DO
       NODE = 0 ! calculation of nodes:
       AN = 0.D0 ! calculation of the norm for the wavefunction
       DO I = 1, N ! F0=0
          AN = AN + F(I)*F(I)
          IF (F(I)*F(I - 1) .LT. 0.D0) NODE = NODE + 1
       END DO
       FSM = F(M + 1) + F(M - 1) - 2.D0*F(M)  ! correction of the energy:
       DE = F(M)*((V(M) - E)*F(M) - FSM/HC)/AN ! 7 step compute deltae
       RETURN
    END
    
    SUBROUTINE PROPAG(N, HC, ISTEP, IINIT, IFIN, IFLAG, F0, F1, E, V, F, M)
       IMPLICIT NONE
       INTEGER(KIND=4):: N, ISTEP, IINIT, IFIN, IFLAG, M, I, I1
       REAL(KIND=8), DIMENSION(0:N):: V, F
       REAL(KIND=8):: HC, F0, F1, E
       F(IINIT - ISTEP) = F0
       F(IINIT) = F1
       DO I = IINIT, IFIN, ISTEP
          I1 = I + ISTEP
          F(I1) = (2.0D0 + HC*(V(I) - E))*F(I) - F(I - ISTEP)
          IF (IFLAG .EQ. ISTEP) THEN
             IF (F(I1) .LT. F(I)) THEN
                M = I1
                RETURN
             END IF
          END IF
       END DO
       RETURN
    END
    
    