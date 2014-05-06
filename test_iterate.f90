PROGRAM ITERATE_TEST
	USE iterate
	IMPLICIT NONE
	open(unit=1, file="DiffusionInput", iostat=ios, status="old", action="read")
	if ( ios /= 0 ) stop "Error opening file name"
	open(unit=2, file="DiffusionOutput", iostat=ios, status="new", action="write")
	if ( ios /= 0 ) stop "Error opening file name"
	REAL, DIMENSION(2,2) :: A
	REAL, DIMENSION(2) :: b, x, x_0
	INTEGER :: n, iter
	REAL :: tol
	CHARACTER :: converge

	A = RESHAPE([2,5,1,7],[2,2])
	b = (11, 13)
	x = (0, 0)
	x_0 = (1,1)
	n = 2
	iter = 1000000
	tol = .00001
	converge = 'z'
	call jacobi(n,A,b,x_0,x,tol,converge,iter)
	print *, x


END PROGRAM ITERATE_TEST