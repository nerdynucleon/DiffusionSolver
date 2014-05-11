PROGRAM Diffusion_Solver
	USE version_data
	USE input
	USE iterate
	USE output
	USE matrix_mod
	IMPLICIT NONE
	INTEGER :: n, n_mat, iter, n_squares, n_circles, i
	REAL :: tol, grid_h
	REAL, ALLOCATABLE, DIMENSION(:) :: D, sig_a, source_mat
	REAL, ALLOCATABLE, DIMENSION(:,:) :: squares, circles, A, F
	REAL, ALLOCATABLE, DIMENSION(:,:,:) :: G
	character(1) :: source_type, converge, T,R,L,B
	! Solution Variables
	REAL, ALLOCATABLE, DIMENSION(:) :: S, x
	REAL :: kay, kay_0

	open(unit=1, file="DiffusionInput", iostat=i, status="old", action="read")
	if ( i /= 0 ) stop "Check that DiffusionInput Exists"
	open(unit=2, file="DiffusionOutput", iostat=i, status="new", action="write")
	if ( i /= 0 ) stop "DiffusionOutput already Exists"
	open(unit=3, file="Geometry.csv", iostat=i, status="new", action="write")
	if ( i /= 0 ) stop "Geometry already Exists"
	open(unit=4, file="matrix.csv", iostat=i, status="new", action="write")
	if ( i /= 0 ) stop "Matrix already Exists"
	open(unit=5, file="Result.csv", iostat=i, status="new", action="write")
	if ( i /= 0 ) stop "Result already Exists"
	
	


! Display Program Information	
	CALL information()
! Input & Error Check


	!Grid Spacing, Number of Materials
	READ(1,*) n, n_mat
	ALLOCATE(G(n-1,n-1,4))
	ALLOCATE(A((n*n), (n*n)))
	ALLOCATE(x(n * n))
	ALLOCATE(S(n * n))
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


! Error Check
	CALL error_check1(n, n_mat)
	CALL error_check2(grid_h, T, R, L, B, iter, tol, converge,source_type)

! Repeat Input
	CALL repeat_input1(n, n_mat,n_squares, n_circles)
	CALL repeat_input2(squares,circles,grid_h,D,sig_a,source_mat,T,R,L,B,iter,tol,converge,source_type)

! Build and Iterate

	CALL geometry(G,n,D,sig_a,source_mat,n_circles, n_squares, squares, circles,grid_h)
	CALL geometry_out(G)

	CALL matrix(n, A, G, L, R, T, B, grid_h, S)
	!CALL matrix_out(A)
	PRINT *, 'Running Solver...'


	CALL solve(n,A,S,x,tol,converge,iter,source_type, kay, kay_0) 
! Output
	CALL solution_out(x, source_type, kay)
	CALL done()

END PROGRAM Diffusion_Solver
