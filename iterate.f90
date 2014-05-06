MODULE iterate

CONTAINS
	SUBROUTINE geometry(G,n,D,sig_a,source_mat)
		IMPLICIT NONE
		INTEGER, INTENT(IN) :: n
		REAL, DIMENSION(:,:,:), INTENT(OUT) :: G
		REAL, DIMENSION(:), INTENT(IN) :: D, sig_a, source_mat
		INTEGER :: i, j
		PRINT *, "Building Geometry..."


		DO i=1, n-1
			DO j=1, n-1

				G(i,j,1)= D(1)
				G(i,j,2)= sig_a(1)
				G(i,j,3)= source_mat(1)
				G(i,j,4)= 0



			END DO
		END DO
	END SUBROUTINE geometry


	SUBROUTINE circle(i,j,grid_h,n_circles,circles,D,sig_a,source_mat,G)
		!IMPLICIT NONE
		!DO k=1, n_circles
		!	IF ( ((i*grid_h - circles(2,k))^2 + ((j*grid_h)-circles(3,k))^2) <= circles(4,k)^2) THEN
		!		G(i,j)= 
		!END DO

	END SUBROUTINE circle


	SUBROUTINE rectangle()
		IMPLICIT NONE

	END SUBROUTINE rectangle



	SUBROUTINE matrix()

	END SUBROUTINE matrix


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
			num = num+1 ! Raise iteration count

			
			DO i = 1, n ! Create new x(i)
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
				error = dot_product(error_vec,error_vec) ! Absolute error
			ELSE
				error = dot_product(error_vec,error_vec)/dot_product(x,x) ! Relative Error
			END IF

			IF (num > iter) THEN
				PRINT *, 'Failed to converge. Consider raising iteration count.'
				STOP
			ELSE

			END IF

			x_0 = x ! Modify Guess

		END DO
	END SUBROUTINE jacobi
END MODULE iterate
