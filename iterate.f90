MODULE iterate

CONTAINS
	SUBROUTINE geometry(G,n,D,sig_a,source_mat,n_circles, n_squares, squares, circles,grid_h)
		IMPLICIT NONE
		INTEGER, INTENT(IN) :: n, n_circles, n_squares
		REAL, DIMENSION(:,:,:), INTENT(OUT) :: G
		REAL, DIMENSION(:), INTENT(IN) :: D, sig_a, source_mat
		REAL, DIMENSION(:,:), INTENT(IN) :: squares, circles
		REAL, INTENT(IN) :: grid_h
		INTEGER :: i, j
		PRINT *, "Building Geometry..."


		DO i=1, n-1
			DO j=1, n-1
				G(i,j,4)= 0
				CALL circle(i,j,grid_h,n_circles,circles,D,sig_a,source_mat,G)
				IF (G(i,j,4) == 0) THEN
					G(i,j,1)= D(1)
					G(i,j,2)= sig_a(1)
					G(i,j,3)= source_mat(1)
				END IF
			END DO
		END DO
	END SUBROUTINE geometry


	SUBROUTINE circle(i,j,grid_h,n_circles,circles,D,sig_a,source_mat,G)
		IMPLICIT NONE
		INTEGER, INTENT(IN) :: i,j,n_circles
		INTEGER :: k, mat
		REAL, DIMENSION(:,:,:), INTENT(INOUT) :: G
		REAL, DIMENSION(:), INTENT(IN) :: D, sig_a, source_mat
		REAL, DIMENSION(:,:), INTENT(IN) :: circles
		REAL :: grid_h
		DO k = 1, n_circles
			IF ( (circles(5,k) > G(i,j,4)) .AND. (radius(circles(2,k),(i*grid_h),circles(3,k),(j*grid_h),circles(4,k)))) THEN
				mat = circles(1,k)
				G(i,j,1)= D(mat)
				G(i,j,2)= sig_a(mat)
				G(i,j,3)= source_mat(mat)
				G(i,j,4)= circles(5,k)
			END IF
		END DO
	END SUBROUTINE circle

	LOGICAL FUNCTION radius(x1,x2,y1,y2,r)
		IMPLICIT NONE
		REAL :: x1,x2,y1,y2,r
		IF (((x1 - x2)**2 + (y1 - y2)**2) <= (r**2) ) THEN
			radius = .TRUE.
		ELSE
			radius = .FALSE.
		END IF
	END FUNCTION radius


	SUBROUTINE rectangle(i,j,grid_h,n_squares,squares,D,sig_a,source_mat,G)
		IMPLICIT NONE
		INTEGER, INTENT(IN) :: i,j,n_squares
		INTEGER :: k, mat
		REAL, DIMENSION(:,:,:), INTENT(INOUT) :: G
		REAL, DIMENSION(:), INTENT(IN) :: D, sig_a, source_mat
		REAL, DIMENSION(:,:), INTENT(IN) :: squares
		REAL :: grid_h
		DO k = 1, n_squares
			IF ((squares(6,k) > G(i,j,4)) .AND. (inside((i*grid_h),(j*grid_h),squares(2,k),squares(3,k),squares(4,k),squares(5,k)))) THEN
				mat = squares(1,k)
				G(i,j,1)= D(mat)
				G(i,j,2)= sig_a(mat)
				G(i,j,3)= source_mat(mat)
				G(i,j,4)= squares(6,k)
			END IF
		END DO
	END SUBROUTINE rectangle



	LOGICAL FUNCTION inside(x,y,x1,x2,y1,y2)
		IF ( (x >= x1) .AND. (x <= x2) .AND. (y >= y1) .AND. (y <= y2) ) THEN
			inside = .TRUE.
		ELSE
			inside = .FALSE.
		END IF
	END FUNCTION inside



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
