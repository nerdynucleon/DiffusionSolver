MODULE iterate

CONTAINS
	SUBROUTINE jacobi(n,A,b,x_0,x,tol,converge,iter)
		IMPLICIT NONE
		INTEGER, INTENT(IN) :: n, iter
		REAL, DIMENSION(:,:), INTENT(IN) :: A
		REAL, DIMENSION(:), INTENT(IN) :: b
		REAL, INTENT(IN) :: tol
		CHARACTER, INTENT(IN) :: converge
		REAl, DIMENSION(:), INTENT(INOUT) :: x, x_0

		INTEGER :: num, i, j
		REAL :: error, sum
		REAL, DIMENSION(n) :: error_vec

		num = 0
		error = tol + 1


		DO WHILE (error > tol)
			num = num+1

			
			DO i = 1, n

				sum = 0

				DO j = 1, n
					IF ( j /= i ) THEN
						sum = sum + A(i,j) * x_0(j)
					END IF 
				END DO

				x(i) = (b(i) - sum)/A(i,i)



			END DO

			error_vec = ABS(x-x_0)

			IF (converge == 'a') THEN
				error = dot_product(error_vec,error_vec)
			ELSE
				error = dot_product(error_vec,error_vec)/dot_product(x,x)

			END IF


			IF (num > iter) THEN
				PRINT *, 'Failed to converge. Consider raising iteration count.'
				STOP
			ELSE

			END IF

			x_0 = x

		END DO
	END SUBROUTINE jacobi
END MODULE iterate
