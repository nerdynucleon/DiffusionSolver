PROGRAM Diffusion_Solver
	USE version_data
	USE input
	USE iterate
	USE output
	IMPLICIT NONE
	INTEGER :: n, n_mat, iter, n_squares, n_circles, i
	REAL :: tol, grid_h
	REAL, ALLOCATABLE, DIMENSION(:) :: D, sig_a, source_mat
	REAL, ALLOCATABLE, DIMENSION(:,:) :: squares, circles
	REAL, ALLOCATABLE, DIMENSION(:,:,:) :: G
	character(1) :: source_type, converge, T,R,L,B

	open(unit=1, file="DiffusionInput", iostat=i, status="old", action="read")
	if ( i /= 0 ) stop "Check that DiffusionInput Exists"
	open(unit=2, file="DiffusionOutput", iostat=i, status="new", action="write")
	if ( i /= 0 ) stop "DiffusionOutput already Exists"
	open(unit=3, file="Geometry.csv", iostat=i, status="new", action="write")
	if ( i /= 0 ) stop "Geometry already Exists"
	open(unit=4, file="Result.csv", iostat=i, status="new", action="write")
	if ( i /= 0 ) stop "Result already Exists"
	
	


! Display Program Information	
	CALL information()
! Input & Error Check


	!Grid Spacing, Number of Materials
	READ(1,*) n, n_mat
	ALLOCATE(G(n-1,n-1,4))
	! Number of Shapes
	READ(1,*) n_squares, n_circles
	! Allocate Material Matrices
	ALLOCATE(D(n_mat))
	ALLOCATE(sig_a(n_mat))
	ALLOCATE(source_mat(n_mat))
	ALLOCATE(squares(6,n_squares))
	ALLOCATE(circles(5,n_circles))
	! Geometry
	READ(1,*) squares
	READ(1,*) circles
	! Grid Spacing
	READ(1,*) grid_h
	! Diffusion Constant Values
	READ(1,*) D
	! Material Cross Section Values
	READ(1,*) sig_a
	! Source
	READ(1,*) source_mat
	! Boundary Conditions
	READ(1,*) T,R,L,B
	! Iteration Properties
	READ(1,*) iter
	READ(1,*) tol
	READ(1,*) converge
	! Source_type
	READ(1,*) source_type

	CALL repeat_input1(n, n_mat,n_squares, n_circles)
	CALL repeat_input2(squares,circles,grid_h,D,sig_a,source_mat,T,R,L,B,iter,tol,converge,source_type)
! 	CALL error_check()
! Build and Iterate
	CALL geometry(G,n,D,sig_a,source_mat,n_circles, n_squares, squares, circles,grid_h)
	CALL geometry_out(G)
!	CALL matrix()
!	CALL iterate()
! Output
	CALL done()

END PROGRAM Diffusion_Solver
