PROGRAM ITERATE_TEST
	USE iterate
	IMPLICIT NONE

	REAL, DIMENSION(2,2) :: A
	REAL, DIMENSION(2) :: b, x
	INTEGER :: n, iter
	REAL :: tol, inf
	CHARACTER :: converge


	inf = huge(1.)
	inf = inf * 2
	print *, inf
	print *, 1/inf
	print *, 0/inf
	
	A = RESHAPE([2,5,1,7],[2,2])
	b = (11, 13)
	x = (1, 1)
	n = 2
	iter = 1000000
	tol = .00001
	converge = 'z'
	call jacobi(n,A,b,x,tol,converge,iter)
	print *, x




END PROGRAM ITERATE_TEST