    module modulefem
    !*********************************************************************
    !    Function:
    !**********************************************************************


    implicit none

    logical, dimension(:), allocatable :: IsFixDof
    integer :: ndtn = 0, ITime = 0
    integer :: NDivX = 0, NDivY = 0
    integer :: NEl = 0, NNod = 0
    integer, dimension(:,:), allocatable :: ICon
    double precision :: gx = 0.D0, gy = 0.D0
    double precision :: Meshdx = 0.D0, Meshdy = 0.D0
    double precision, dimension(:), allocatable :: GrvF, InrF, Mas, Area, Areai, ExtF, v0, v, dis, DamF
    double precision, dimension(:,:), allocatable :: NodCo
    double precision, dimension(:,:,:), allocatable :: SigG, EpsG, F, HS, B_trial, EpsP, EpsE, edP, ETA, Sig0, &
        Statevar, Swp, Dswp
    double precision, dimension(:,:,:,:), allocatable :: B
    double precision :: MatProp(3), dt = 0.D0, epsv
    double precision :: BulkW = 2200000.d0 ! Bulk Modulus of Water 2200000 kN/m2
    
    integer :: NPLAST = 0
    double precision :: fbar = 0.d0,plastind = 0.d0

    integer, parameter :: DATUnit = 2
    integer, parameter :: LOGUnit = 2
    integer, parameter :: MSHUnit = 3
    integer, parameter :: RESUnit = 4

    contains

    subroutine solve()
    !**********************************************************************
    !    Function:
    !**********************************************************************

    implicit none
    integer :: IIter, IStep = 0, IPTime, dPsi
    integer :: iprint, imeshwrite
    iprint = 100
    imeshwrite = 10000.0
    open(LOGUnit, file = 'output.log')
    call initial()
    call WtMSH(MSHUnit)


    do ITime = 1, ndtn ! physical time scale

        call Map2nod()
        call Update()

        !if (itime.eq.1 .or. int(ITime/5000.0) .eq. (ITime/5000.0)) then
        if(itime.eq.1 .or. (itime/imeshwrite)*imeshwrite == itime .or. itime == ndtn) then
            IStep = IStep + 1
            call WtRES(IStep)
        endif
        if(itime.eq.1 .or. (itime/iprint)*iprint == itime .or. itime == ndtn) then

            write (*, *) -epsg(2,1,1),',',-sigg(2,1,1)+sigg(1,1,1),',',epsg(1,1,1)+epsg(2,1,1)
            write (LOGUnit, *) -epsg(2,1,1),',',-sigg(2,1,1)+sigg(1,1,1),',',epsg(1,1,1)+epsg(2,1,1)
        endif
    enddo

    read(*,*)
    close(2)

    end subroutine solve

    subroutine readdata()
    !**********************************************************************
    !    Function: Make a file
    !**********************************************************************

    implicit none
    double precision :: r1(2), r2(2), LPos(2), temp, Gpy, PI
    integer :: I, J, IEl, INod(4), ig

    open(DATUnit, file = 'input.dat')
    read(DATUnit, *) NDivX, NDivY
    read(DATUnit, *) Meshdx, Meshdy
    NNod = (NDivX + 1)*(NDivY + 1)
    NEl = NDivX * NDivY
    allocate (NodCo(2, NNod)); nodco = 0.d0
    allocate (ICon(4, NEl)); icon = -1
    allocate (IsFixDof(2 * NNod))
    allocate(Mas(2 * NNod)); Mas = 0.d0
    allocate(GrvF(2 * NNod)); GrvF = 0.d0
    allocate(InrF(2 * NNod)); InrF = 0.d0
    allocate(ExtF(2 * NNod)); ExtF = 0.d0
    allocate(DamF(2 * NNod)); DamF = 0.d0
    allocate(v(2 * NNod)); V = 0.d0
    allocate(v0(2 * NNod)); V0 = 0.d0
    allocate(dis(2 * NNod)); dis = 0.d0
    allocate(B(2, 4, NEl, 4)); B = 0.d0
    allocate(Area(NEl)); Area = 0.d0
    allocate(Areai(NEl)); Areai = 0.d0
    allocate(Sigg(4, 4, NEl)); Sigg = 0.d0
    allocate(Sig0(4, 4, NEl));  Sig0 = 0.d0
    Sigg(1,:,:) = -100.d0
    Sigg(2,:,:) = -100.d0
    Sigg(4,:,:) = -100.d0
    allocate(DSwp(4, 4, NEl)); DSwp = 0.d0
    allocate(EpsP(4, 4, NEl)); EpsP = 0.d0
    allocate(edP(1 ,4, NEl)); edP = 0.d0
    allocate(EpsE(3, 4, NEl)); EpsE = 0.d0
    allocate(eta(1,4,nel)); eta =0.d0
    allocate(EpsG(3, 4, NEl)); EpsG = 0.d0
    allocate(F(4, 4, nel)); F(1,:,:) = 1.d0; F(2,:,:) = 1.d0; F(3,:,:) = 0.d0; F(4,:,:) = 0.d0
    allocate(Statevar(32,4,Nel)); Statevar = 0.d0 ! State variable


    allocate(HS(4, NEL, 4)); HS = 0.d0

    do I = 1, NNod
        read(DATUnit, *) temp, NodCo(1, I), NodCo(2, I), temp
    end do

    do I = 1, NEl
        read(DATUnit, *) temp, ICon(1, I), ICon(2, I), ICon(3, I), ICon(4, I)
    end do

    read(DATUnit, *) MatProp(1), MatProp(2), MatProp(3)

    read(DATUnit, *) gx, gy
    read(DATUnit, *) ndtn, dt
    close(1)
    !Bulkw = MatProp(4)
    do IEl = 1, NEl
        INod(:) = ICon(:, IEl)
        !Area(IEl) = abs(NodCo(1,INod(3))-NodCo(1,INod(1)))* abs(NodCo(2,INod(3))-NodCo(2,INod(1)))
    end do

    do I=1,nnod
        NodCo(1,I) = NodCo(1,I) * Meshdx
        NodCo(2,I) = NodCo(2,I) * Meshdy
    enddo
    ! Boundary Conditions
    IsFixDof = .false.
    IsFixDof(1) = .true.
    IsFixDof(2) = .true.
    IsFixDof(3) = .true.
    IsFixDof(4) = .true.
    IsFixDof(5) = .true.
    IsFixDof(7) = .true.

    !do I = 1, NNod ! vertical bar problem
    !    if ((NodCo(2, I) .eq. 0.d0)) then
    !        IsFixDof((I - 1) * 2 + 1) = .true.
    !        IsFixDof((I - 1) * 2 + 2) = .true.
    !    end if
    !    if ((NodCo(1, I) .eq. 0.1d0)) then
    !        IsFixDof((I - 1) * 2 + 1) = .true.
    !    end if
    !    if ((NodCo(1, I) .eq. 0.d0)) then
    !        IsFixDof((I - 1) * 2 + 1) = .true.
    !    end if ! Base Fixed
    !
    !    !    !if ((NodCo(1, I) .eq. 1.d0).and.((NodCo(2, I) .eq. 0.d0))) then
    !    !    !    IsFixDof((I - 1) * 2 + 1) = .true.
    !    !    !end if
    !    !    !if ((NodCo(1, I) .eq. 1.d0).and.((NodCo(2, I) .eq. 5.d0))) then
    !    !    !    IsFixDof((I - 1) * 2 + 1) = .true.
    !    !    !end if
    !    !
    !end do

    WRITE(*,*) 'Youngs Modulus = ', MatProp(1)
    WRITE(*,*) 'Poissons ration = ', MatProp(2)
    WRITE(*,*) 'Mass = ', MatProp(3)
    WRITE(*,*) 'Gravity in Y Direction = ', gy
    READ(*,*)

    end subroutine readdata

    subroutine Initial()

    !**********************************************************************
    !    Function: Calculates B-Strain Displacement matrix (1 and 4 gauss points)
    !**********************************************************************

    implicit none

    integer :: IEl, I, J, K, INod(4), Id, ig
    double precision :: LPos(2, 1), dNxi(2, 4), Ja(2, 2), JaI(2, 2), A
    double precision :: xi, eta, rp, rm, sp, sm, temp

    temp = 1.d0/sqrt(3.d0)
    !temp=0.d0

    B = 0.0d0

    do IEl = 1, nel
        INod(:) = ICon(:, IEl)

        do ig = 1, 4

            select case (ig)
            case(1)
                xi = -temp
                eta = -temp

            case (2)
                xi = temp
                eta = -temp

            case(3)
                xi = temp
                eta = temp

            case(4)
                xi = -temp
                eta = temp
            end select

            rp = 1.0 + xi
            rm = 1.0 - xi;
            sp = 1.0 + eta;
            sm = 1.0 - eta;

            dNxi(1, 1) = -0.25D0 * sm; dNxi(1, 2) = +0.25D0 * sm; dNxi(1, 3) = +0.25D0 * sp
            dNxi(1, 4) = -0.25D0 * sp; dNxi(2, 1) = -0.25D0 * rm; dNxi(2, 2) = -0.25D0 * rp
            dNxi(2, 3) = +0.25D0 * rp; dNxi(2, 4) = +0.25D0 * rm

            HS(1, iel, ig) = (1.D0 - xi)*(1.D0 - eta)/4.D0
            HS(2, iel, ig) = (1.D0 + xi)*(1.D0 - eta)/4.D0
            HS(3, iel, ig) = (1.D0 + xi)*(1.D0 + eta)/4.D0
            HS(4, iel, ig) = (1.D0 - xi)*(1.D0 + eta)/4.D0

            Area(IEl) = abs(NodCo(1, INod(3)) - NodCo(1, INod(1))) * abs(NodCo(2, INod(3)) - NodCo(2, INod(1)))

            Ja = 0.0D0

            do I = 1, 2
                do J = 1, 2
                    do K = 1, 4
                        Ja(I, J) = Ja(I, J) + dNxi(I, K) * NodCo(J, INod(K))
                    end do
                end do
            end do
            A = Ja(1, 1) * Ja(2, 2) - Ja(1, 2) * Ja(2, 1)

            if (A .gt. 0.D0) then
                JaI(1, 1) = +Ja(2, 2)/A; JaI(1, 2) = -Ja(1, 2)/A
                JaI(2, 1) = -Ja(2, 1)/A; JaI(2, 2) = +Ja(1, 1)/A
            else
                write(LOGUnit, *) 'negative or zero Jacobian !!'; stop
            end if

            do J = 1, 4
                B(1, J, IEl, ig) = dNxi(1, J) * JaI(1, 1) + dNxi(2, J) * JaI(1, 2)
                B(2, J, IEl, ig) = dNxi(1, J) * JaI(2, 1) + dNxi(2, J) * JaI(2, 2)
            end do
        enddo
    enddo

    ! call Map2Nod()

    end subroutine Initial

    subroutine Map2Nod()

    !**********************************************************************
    !    Function: Internal and External Forces, Mass at each nodes
    !**********************************************************************

    implicit none
    integer :: I, IEl, Id, INod(4), ig, J
    double precision :: factor
    GrvF = 0.D0; InrF = 0.D0; ExtF = 0.D0; Mas = 0.D0

    ExtF(5) =  50.d0
    ExtF(7) = -50.d0
    ExtF(1) =  50.d0
    ExtF(3) = -50.d0

    do IEl = 1, nel
        INod(:) = ICon(:, IEl)
        do ig = 1, 4 ! gauss
            do I = 1, 4
                Id = (INod(I) - 1) * 2
                !!InrF(Id + 1) = InrF(Id + 1)+ ((Sigg(1,ig,IEl)+Swp(1,ig,Iel)) * B(1,I,iel,ig) + (Sigg(3,ig,IEl))               * B(2, I, iel, ig)) * Area(iel)/4.d0
                !!InrF(Id + 2) = InrF(Id + 2)+ ((Sigg(3,ig,IEl))               * B(1,I,iel,ig) + (Sigg(2,ig,IEl)+Swp(2,ig,iel)) * B(2, I, iel, ig)) * Area(iel)/4.d0
                InrF(Id + 1) = InrF(Id + 1)+ ((Sigg(1,ig,IEl)) * B(1,I,iel,ig) + (Sigg(3,ig,IEl)) * B(2, I, iel, ig)) * Area(iel)/4.d0
                InrF(Id + 2) = InrF(Id + 2)+ ((Sigg(3,ig,IEl)) * B(1,I,iel,ig) + (Sigg(2,ig,IEl)) * B(2, I, iel, ig)) * Area(iel)/4.d0
                GrvF(Id + 1) = GrvF(Id + 1) + Area(iel) * MatProp(3) * hs(i, iel, ig) * gx /4.d0
                GrvF(Id + 2) = GrvF(Id + 2) + Area(iel) * MatProp(3) * hs(i, iel, ig) * gy /4.d0 !* factor
                Mas(Id + 1) = Mas(Id + 1) + Area(iel) * MatProp(3) * hs(i, iel, ig) /4.d0
                Mas(Id + 2) = Mas(Id + 2) + Area(iel) * MatProp(3) * hs(i, iel, ig) /4.d0
            end do
        enddo !gauss
    end do !nel
    end subroutine Map2Nod

    subroutine  Update()
    !**********************************************************************
    !    Function: Small deformation formulation
    !*************************** *******************************************

    implicit none
    integer :: IEl, INod(4), Id, I, ig, J
    double precision :: delV(4), deps(4), tem, dampf = 0.00001d0, check, epsilon, rad
    

    do I = 1, NNod
        Id = (I - 1) * 2
        if (.not.IsFixDof(Id + 1).and.(Mas(Id + 1) .gt. 0.D0)) then
            tem = V0(Id + 1)+(GrvF(Id + 1) + ExtF(Id + 1) - InrF(Id + 1))/Mas(Id + 1) * dt
            V(Id + 1) = tem - sign(1.d0, tem) * dampf * abs(tem)
        end if

        if (.not.IsFixDof(Id + 2).and.(Mas(Id + 2) .gt. 0.D0)) then
            tem = V0(Id + 2)+(GrvF(Id + 2) + ExtF(Id + 2) - InrF(Id + 2))/Mas(Id + 2) * dt
            V(Id + 2) = tem - sign(1.d0, tem) * dampf * abs(tem)
        end if
    end do

    v(6) = -0.25d0 !* sin(2.d0*atan(1.d0)*1.d0*itime*dt* 4.d0)
    v(8) = -0.25d0 !* sin(2.d0*atan(1.d0)*1.d0*itime*dt* 4.d0)

    do IEl = 1, NEl
        INod(:) = ICon(:, IEl)
        do ig = 1, 4 ! gauss
            delV = 0.0
            do I = 1, 4
                Id = (INod(I) - 1) * 2
                delV(1) = delV(1) + B(1, i, iel, ig) * V(Id + 1)
                delV(2) = delV(2) + B(2, i, iel, ig) * V(Id + 2)
                delV(3) = delV(3) + B(2, i, iel, ig) * V(Id + 1)
                delV(4) = delV(4) + B(1, i, iel, ig) * V(Id + 2)
            end do
            dEps(1:2) = delV(1:2) * dt
            dEps(3) = (delV(3) + delV(4)) * dt

            EpsG(1:2, ig, IEl) = EpsG(1:2, ig, IEl) +  dEps(1:2)
            EpsG(3, ig, IEl) = EpsG(3, ig, IEl) +  dEps(3)

            !EpsV = deps(1) + deps(2)                              ! Volumetric strain, for Undrained analysis
            !Dswp(1:4,ig,iel) = BulkW * EpsV                       ! Incremental water pressure, for Undrained analysis
            !Swp(1:4,ig,iel) = Swp(1:4,ig,iel) + Dswp(1:4,ig,iel)  ! Total water pressure, for Undrained analysis
            !enddo !gauss

            !do ig = 1, 4
            call Elastic(MatProp(1),MatProp(2), deps(:), Sigg(:,ig,IEl)) ! Elastic material
            call MOHRC(SIN(30.d0*0.0174533D0), SIN(0.d0*0.0174533D0), 1.d0, MatProp(1), MatProp(2), SigG(:,ig,iel), NPLAST, FBAR, PLASTIND)
        enddo !gauss
    end do ! particles

    dis = dis+ v* dt
    v0 = v



    end subroutine Update

    !*********************************************************
    !!Elastic Hookes model
    !!**********************************************************
    subroutine Elastic(E, nu, eps, Sig)

    implicit none
    !
    double precision, intent(in) :: E, nu
    double precision, intent(inout) :: Sig(8)
    double precision, intent(in) :: eps(3)
    ! local variables
    double precision :: G_mod, K_mod, Eps_tr

    G_mod = E / (2.d0 * (1 + nu))
    K_mod = E / (3.d0 * (1 - 2 * nu))

    Eps_tr = eps(1) + eps(2)
    Sig(1) = sig(1) +  ((K_mod * Eps_tr) + 2 * G_mod * (eps(1) - (Eps_tr/3.d0)))
    Sig(2) = sig(2) +  ((K_mod * Eps_tr) + 2 * G_mod * (eps(2) - (Eps_tr/3.d0)))
    Sig(3) = sig(3) +  (2 * G_mod * eps(3))
    Sig(4) = sig(4) +  ((K_mod * Eps_tr) + 2 * G_mod * (0.d0 - (Eps_tr/3.d0)))

    !Sig(3) = sig(3) +  ((K_mod * Eps_tr) + 2 * G_mod * (0.d0 - (Eps_tr/3.d0)))
    !Sig(4) = sig(4) +  (2 * G_mod * eps(3))

    endsubroutine elastic

    subroutine MkOpFiles()
    !**********************************************************************
    !    Function: Output files
    !**********************************************************************
    implicit none

    call MkFile(MSHUnit, 'Model.post.msh')
    call MkFile(RESUnit, 'Model.post.res')

    end subroutine MkOpFiles


    subroutine MkFile(Unit, flNam)
    !**********************************************************************
    !
    !    Function: Make a file
    !
    !**********************************************************************

    implicit none

    integer Unit
    character flNam * (*)

    if (FlExist(flNam)) then
        open(Unit, file = flNam)
        close(Unit, Status = 'Delete')
    endif
    open(Unit, file = flNam)

    end subroutine MkFile

    logical function FlExist(flNam)
    !**********************************************************************
    !
    !    Function: To check the existence of a file
    !
    !**********************************************************************

    implicit none

    logical lfil
    character flNam * (*)

    lfil = .false.
    inquire(file = flNam, exist = lfil)
    if (lfil) then
        FlExist = .true.
    else
        FlExist = .false.
    endif

    end function FlExist


    subroutine WtMSH(Unit)
    !**********************************************************************
    !    Function: Writing GiD *.msh file
    !!**********************************************************************

    implicit none
    integer Unit
    ! local variables
    integer :: IPart, INod, IEl, J, K
    double precision, dimension(2) :: Pos, r1, r2, VPos

    write(Unit, *) 'MESH dimension 2 ElemType Quadrilateral Nnode 4'
    write(Unit, *) 'Coordinates'
    do INod = 1, NNod
        write(Unit, 111) INod, NodCo(1, INod), NodCo(2, INod)
    end do
    write(Unit, *) 'End Coordinates'
    write(Unit, *) 'Elements'
    do IEl = 1, NEl
        write(Unit, "(5I7)") IEl, ICon(1, IEl), ICon(2, IEl), ICon(3, IEl), ICon(4, IEl)
    end do
    write(Unit, *) 'End Elements'
    close(Unit)
111 format(I7, 2E16.6E3)

    end subroutine WtMSH

    subroutine WtRES(IStep)
    !**********************************************************************
    !
    !    Function:
    !
    !**********************************************************************

    implicit none

    integer IStep
    ! local variables
    integer :: IEl, J, K, INod, Id, Unit, ig

    Unit = ResUnit
    if (IStep .eq. 1) then
        write(Unit, *) 'GiD Post Results File 1.0'
        write(Unit, *) 'GaussPoints "Material_Point" Elemtype Quadrilateral'
        write(Unit, *) 'Number of Gauss Points: 4'
        write(Unit, *) 'Natural Coordinates: Internal'
        write(Unit, *) 'end gausspoints'
        write(Unit, *) 'Result "Boundary" "MPM"', IStep, 'Vector OnNodes'
        write(Unit, *) 'ComponentNames "X-fix", "Y-fix"'
        write(Unit, *) 'values'
        do INod = 1, NNod
            Id = (INod - 1) * 2
            J = 0; K = 0
            if (IsFixDof(Id + 1)) J = 1
            if (IsFixDof(Id + 2)) K = 1
            write(Unit, "(3I7)") INod, J, K
        end do
        write(Unit, *) 'end values'
    end if
    write(Unit, *) 'Result "displacement" "MPM"', IStep, 'Vector OnNodes'
    write(Unit, *) 'ComponentNames "comp. x", "comp. y"'
    write(Unit, *) 'values'
    do INod = 1, NNod
        Id = (INod - 1) * 2
        write(Unit, "(I7, 2E16.6E3)") INod, Dis(Id + 1), Dis(Id + 2)
    end do
    write(Unit, *) 'end values'

    write(Unit, *) 'Result "Internal_force" "MPM"', IStep, 'Vector OnNodes'
    write(Unit, *) 'ComponentNames "Inrf. x", "Inrf. y"'
    write(Unit, *) 'values'
    do INod = 1, NNod
        Id = (INod - 1) * 2
        write(Unit, "(I7, 2E16.6E3)") INod, Inrf(Id + 1), Inrf(Id + 2)
    end do
    write(Unit, *) 'end values'

    write(Unit, *) 'Result "Gravity_force" "MPM"', IStep, 'Vector OnNodes'
    write(Unit, *) 'ComponentNames "Grvf. x", "Grvf. y"'
    write(Unit, *) 'values'
    do INod = 1, NNod
        Id = (INod - 1) * 2
        write(Unit, "(I7, 2E16.6E3)") INod, Grvf(Id + 1), Grvf(Id + 2)
    end do
    write(Unit, *) 'end values'

    write(Unit, *) 'Result "Mass" "MPM"', IStep, 'Vector OnNodes'
    write(Unit, *) 'ComponentNames "Mass. x", "Mass. y"'
    write(Unit, *) 'values'
    do INod = 1, NNod
        Id = (INod - 1) * 2
        write(Unit, "(I7, 2E16.6E3)") INod, Mas(Id + 1), Mas(Id + 2)
    end do
    write(Unit, *) 'end values'

    write(Unit, *) 'Result "Stress" "MPM"', IStep, 'Vector OnGaussPoints "Material_Point"'
    write(Unit, *) 'ComponentNames "sigma xx", "sigma yy", "sigma zz", "sigma xy"'
    write(Unit, *) 'values'
    do IEl = 1, NEl
        write(Unit, "(I7, 4E16.6E4)") IEl, Sigg(1, 1, IEl), Sigg(2, 1, IEl), Sigg(4, 1, IEl),Sigg(3, 1, IEl)
        write(Unit, "(4E16.6E4)") Sigg(1, 2, IEl), Sigg(2, 2, IEl), Sigg(4, 1, IEl),Sigg(3, 2, IEl)
        write(Unit, "(4E16.6E4)") Sigg(1, 3, IEl), Sigg(2, 3, IEl),Sigg(4, 1, IEl), Sigg(3, 3, IEl)
        write(Unit, "(4E16.6E4)") Sigg(1, 4, IEl), Sigg(2, 4, IEl), Sigg(4, 1, IEl),Sigg(3, 4, IEl)
    end do
    write(Unit, *) 'end values'

    write(Unit, *) 'Result "Strain" "MPM"', IStep, 'Vector OnGaussPoints "Material_Point"'
    write(Unit, *) 'ComponentNames "eps xx", "eps yy", "eps xy"'
    write(Unit, *) 'values'
    do IEl = 1, NEl
        write(Unit, "(I7, 3E16.6E3)") IEl, EpsG(1, 1, IEl), EpsG(2, 1, IEl), EpsG(3, 1, IEl)
        write(Unit, "( 3E16.6E3)") Epsg(1, 2, IEl), EpsG(2, 2, IEl), EpsG(3, 2, IEl)
        write(Unit, "( 3E16.6E3)") Epsg(1, 3, IEl), Epsg(2, 3, IEl), Epsg(3, 3, IEl)
        write(Unit, "( 3E16.6E3)") Epsg(1, 4, IEl), Epsg(2, 4, IEl), Epsg(3, 4, IEl)
    end do
    write(Unit, *) 'end values'


    write(Unit, *) 'Result "Plastic strain" "MPM"', IStep, 'Vector OnGaussPoints "Material_Point"'
    write(Unit, *) 'ComponentNames "EpsP xx", "EpsP yy", "EpsP zz", "EpsP xy"'
    write(Unit, *) 'values'
    do IEl = 1, NEl
        write(Unit, "(I7, 4E16.6E4)") IEl, Sigg(1, 1, IEl), Sigg(2, 1, IEl), Sigg(3, 1, IEl), Sigg(4, 1, IEl)
        write(Unit, "(4E16.6E4)") Sigg(1, 2, IEl), Sigg(2, 2, IEl), Sigg(3, 2, IEl), Sigg(4, 1, IEl)
        write(Unit, "(4E16.6E4)") Sigg(1, 3, IEl), Sigg(2, 3, IEl), Sigg(3, 3, IEl),Sigg(4, 1, IEl)
        write(Unit, "(4E16.6E4)") Sigg(1, 4, IEl), Sigg(2, 4, IEl), Sigg(3, 4, IEl),Sigg(4, 1, IEl)
    end do
    write(Unit, *) 'end values'


    end subroutine WtRES

    end module modulefem

    !************program *****************
    program FEM2D

    use modulefem
    implicit none

    call MkOpFiles()
    call readdata()

    call solve()

    end program
    !*************program*****************