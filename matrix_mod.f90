MODULE matrix_mod

CONTAINS
	SUBROUTINE matrix(n, A, G, L, R, T, B, grid_h, S)
		IMPLICIT NONE
		INTEGER :: k, i, j, m
		INTEGER, INTENT(IN) :: n
		REAL, DIMENSION(:,:), INTENT(INOUT) :: A
		REAL, DIMENSION(:,:,:), INTENT(IN) :: G
		CHARACTER, INTENT(IN) :: L, R, T, B
		REAL, INTENT(IN) :: grid_h
		REAL, DIMENSION(:), INTENT(INOUT) :: S

		PRINT *, "Building Matrix 'A'..."



		!BODY
		DO k = (n + 1) , (n * (n - 1))
			CALL indices(i, j, k, n)
			!CHECK IF LEFT
			IF (i == 0) THEN
				CALL left_conditions(i, j, k, n, G, A, grid_h, L, S)
				CYCLE
			!CHECK IF RIGHT
			ELSE IF (i == (n - 1)) THEN
				CALL right_conditions(i, j, k, n, G, A, grid_h, R, S)
				CYCLE
			!Default
			ELSE
				CALL general_conditions(i, j, k, n, G, A, grid_h, S)

			END IF
		END DO

		!BOTTOM
		DO k = 2, n-1
			CALL indices(i, j, k, n)
			CALL bottom_conditions(i, j, k, n, G, A, grid_h, B, S)
		END DO

		!TOP
		DO k = ((n * (n - 1)) + 2), ((n * n) - 1)
			CALL indices(i, j, k, n)
			CALL top_conditions(i, j, k, n, G, A, grid_h, T, S)
		END DO

		!Corners
		CALL corners(n, G, A, grid_h, L, T, R, B, S)

	END SUBROUTINE matrix

	SUBROUTINE corners(n, G, A, grid_h, L, T, R, B, S)
		IMPLICIT NONE
		REAL :: left, right, bottom, top, loss, center
		INTEGER :: i, j, k
		INTEGER, INTENT(IN) :: n
		REAL, DIMENSION(:,:,:), INTENT(IN) :: G
		REAL, DIMENSION(:,:), INTENT(INOUT) :: A
		REAL, INTENT(IN) :: grid_h
		CHARACTER, INTENT(IN) :: L, T, R, B
		REAL, DIMENSION(:), INTENT(INOUT) :: S


		! Bottom Left
		k = 1
		CALL indices(i, j, k, n)

		!grid h kept for expandibility
		left = 0
		right = -( G(i + 1, j + 1, 1)*grid_h)/(grid_h)
		bottom = 0
		top = -( G(i + 1, j + 1, 1)*grid_h)/(grid_h)
		loss = ( G(i + 1,j + 1, 2)*grid_h*grid_h )/4
		center = loss - (left + right + bottom + top)
		!Assigning Values
		A(k, k) = center
		A(k + 1, k) = right
		A(k + n, k) = top

		!Source
		IF (B == 'R') THEN
			center = ( G(i + 1, j + 1, 3)*grid_h*grid_h )/4
			S(k) = center
		ELSE
			S(k) = 0
		END IF


		! Botton Right
		k = n
		CALL indices(i, j, k, n)
		!grid h kept for expandibility
		left = -( G(i, j + 1, 1)*grid_h)/(grid_h)
		right = 0
		bottom = 0
		top = -( G(i, j + 1, 1)*grid_h )/(grid_h)
		loss = ( G(i, j + 1, 2)*grid_h*grid_h)/4
		center = loss - (left + right + bottom + top)

		!Assigning Values
		A(k, k) = center
		A(k - 1, k) = left
		A(k + n, k) = top

		!Source
		IF (R == 'R') THEN
			center = (G(i, j + 1, 3)*grid_h*grid_h)/4
			S(k) = center
		ELSE
			S(k) = 0
		END IF

		! Top Left
		k = ((n * (n - 1)) + 1)
		CALL indices(i, j, k, n)
		!grid h kept for expandibility
		left = 0
		right = -( G(i + 1, j, 1)*grid_h )/(grid_h)
		bottom = -( G(i + 1, j, 1)*grid_h)/(grid_h)
		top = 0
		loss = (G(i + 1, j, 2)*grid_h*grid_h )/4
		center = loss - (left + right + bottom + top)

		!Assigning Values
		A(k, k) = center
		A(k + 1, k) = right
		A(k - n, k) = bottom
		!Source
		IF (L == 'R') THEN
			center = ( G(i + 1, j, 3)*grid_h*grid_h )/4
			S(k) = center
		ELSE
			S(k) = 0
		END IF


		! Top Right
		k = n * n
		CALL indices(i, j, k, n)

		!grid h kept for expandibility
		left = -( G(i, j, 1)*grid_h )/(grid_h)
		right = 0
		bottom = -( G(i, j, 1)*grid_h )/(grid_h)
		top = 0
		loss = (G(i, j, 2)*grid_h*grid_h )/4
		center = loss - (left + right + bottom + top)

		!Assigning Values
		A(k, k) = center
		A(k - 1, k) = left
		A(k - n, k) = bottom
		!Source

		IF (T == 'R') THEN
			center = ( G(i + 1, j + 1, 3)*grid_h*grid_h )/4
			S(k) = center
		ELSE
			S(k) = 0
		END IF

	END SUBROUTINE corners

	SUBROUTINE left_conditions(i, j, k, n, G, A, grid_h, L, S)
		IMPLICIT NONE
		REAL :: left, right, bottom, top, loss, center
		INTEGER, INTENT(IN) :: i, j, k, n
		REAL, DIMENSION(:,:,:), INTENT(IN) :: G
		REAL, DIMENSION(:,:), INTENT(INOUT) :: A
		REAL, INTENT(IN) :: grid_h
		CHARACTER, INTENT(IN) :: L
		REAL, DIMENSION(:), INTENT(INOUT) :: S

		!grid h kept for expandibility
		left = 0
		right = -( G(i + 1, j, 1)*grid_h + G(i + 1, j + 1, 1)*grid_h)/(2*grid_h)
		bottom = -( G(i + 1, j, 1)*grid_h)/(grid_h)
		top = -( G(i + 1,j + 1, 1)*grid_h)/(grid_h)
		loss = (G(i + 1, j, 2)*grid_h*grid_h + G(i + 1,j + 1,2)*grid_h*grid_h)/4
		center = loss - (left + right + bottom + top)

		!Assigning Values
		A(k, k) = center
		A(k + 1, k) = right
		A(k - n, k) = bottom
		A(k + n, k) = top
		!Source
		IF (L == 'R') THEN
			center = ( G(i + 1, j, 3)*grid_h*grid_h + G(i + 1, j + 1, 3)*grid_h*grid_h )/4
			S(k) = center
		ELSE
			S(k) = 0
		END IF
	END SUBROUTINE left_conditions

	SUBROUTINE right_conditions(i, j, k, n, G, A, grid_h, R, S)
		IMPLICIT NONE
		REAL :: left, right, bottom, top, loss, center
		INTEGER, INTENT(IN) :: i, j, k, n
		REAL, DIMENSION(:,:,:), INTENT(IN) :: G
		REAL, DIMENSION(:,:), INTENT(INOUT) :: A
		REAL, INTENT(IN) :: grid_h
		CHARACTER, INTENT(IN) :: R
		REAL, DIMENSION(:), INTENT(INOUT) :: S
		!grid h kept for expandibility
		left = -( G(i, j, 1)*grid_h + G(i, j + 1, 1)*grid_h)/(2*grid_h)
		right = 0
		bottom = -( G(i, j, 1)*grid_h )/(grid_h)
		top = -( G(i, j + 1, 1)*grid_h )/(grid_h)
		loss = (G(i, j, 2)*grid_h*grid_h + G(i, j + 1, 2)*grid_h*grid_h)/4
		center = loss - (left + right + bottom + top)

		!Assigning Values
		A(k, k) = center
		A(k - 1, k) = left
		A(k - n, k) = bottom
		A(k + n, k) = top
		!Source
		IF (R == 'R') THEN
			center = (G(i, j, 3)*grid_h*grid_h + G(i, j + 1, 3)*grid_h*grid_h)/4
			S(k) = center
		ELSE
			S(k) = 0
		END IF

	END SUBROUTINE right_conditions

	SUBROUTINE bottom_conditions(i, j, k, n, G, A, grid_h, B, S)
		IMPLICIT NONE
		REAL :: left, right, bottom, top, loss, center
		INTEGER, INTENT(IN) :: i, j, k, n
		REAL, DIMENSION(:,:,:), INTENT(IN) :: G
		REAL, DIMENSION(:,:), INTENT(INOUT) :: A
		REAL, INTENT(IN) :: grid_h
		CHARACTER, INTENT(IN) :: B
		REAL, DIMENSION(:), INTENT(INOUT) :: S
		!grid h kept for expandibility
		left = -(  G(i, j + 1, 1)*grid_h)/(grid_h)
		right = -( G(i + 1, j + 1, 1)*grid_h)/(grid_h)
		bottom = 0
		top = -( G(i, j + 1 ,1)*grid_h + G(i + 1, j + 1, 1)*grid_h)/(2*grid_h)
		loss = (G(i + 1,j + 1, 2)*grid_h*grid_h + G(i, j + 1, 2)*grid_h*grid_h)/4
		center = loss - (left + right + bottom + top)

		!Assigning Values
		A(k, k) = center
		A(k - 1, k) = left
		A(k + 1, k) = right
		A(k + n, k) = top
		!Source
		IF (B == 'R') THEN
			center = ( G(i + 1, j + 1, 3)*grid_h*grid_h + G(i, j + 1, 3)*grid_h*grid_h)/4
			S(k) = center
		ELSE
			S(k) = 0
		END IF

	END SUBROUTINE bottom_conditions

	SUBROUTINE top_conditions(i, j, k, n, G, A, grid_h, T, S)
		IMPLICIT NONE
		REAL :: left, right, bottom, top, loss, center
		INTEGER, INTENT(IN) :: i, j, k, n
		REAL, DIMENSION(:,:,:), INTENT(IN) :: G
		REAL, DIMENSION(:,:), INTENT(INOUT) :: A
		REAL, INTENT(IN) :: grid_h
		CHARACTER, INTENT(IN) :: T
		REAL, DIMENSION(:), INTENT(INOUT) :: S
		!grid h kept for expandibility
		left = -( G(i, j, 1)*grid_h )/(grid_h)
		right = -( G(i + 1, j, 1)*grid_h )/(grid_h)
		bottom = -( G(i, j, 1)*grid_h + G(i + 1, j, 1)*grid_h)/(2*grid_h)
		top = 0
		loss = (G(i, j, 2)*grid_h*grid_h + G(i + 1, j, 2)*grid_h*grid_h)/4
		center = loss - (left + right + bottom + top)

		!Assigning Values
		A(k, k) = center
		A(k - 1, k) = left
		A(k + 1, k) = right
		A(k - n, k) = bottom

		!Source
		IF (T == 'R') THEN
			center = (G(i, j, 3)*grid_h*grid_h + G(i + 1, j, 3)*grid_h*grid_h )/4
			S(k) = center
		ELSE
			S(k) = 0
		END IF

	END SUBROUTINE top_conditions


	SUBROUTINE general_conditions(i, j, k, n, G, A, grid_h, S)
		IMPLICIT NONE
		REAL :: left, right, bottom, top, loss, center
		INTEGER, INTENT(IN) :: i, j, k, n
		REAL, DIMENSION(:,:,:), INTENT(IN) :: G
		REAL, DIMENSION(:,:), INTENT(INOUT) :: A
		REAL, INTENT(IN) :: grid_h
		REAL, DIMENSION(:), INTENT(INOUT) :: S

		!Source
		center=(G(i, j, 3)*grid_h*grid_h + G(i + 1, j, 3)*grid_h*grid_h + G(i +1, j +1, 3)*grid_h*grid_h + G(i, j +1, 3)*grid_h*grid_h)/4
		S(k) = center

		!grid h kept for expandibility
		left = -( G(i, j, 1)*grid_h + G(i, j + 1, 1)*grid_h)/(2*grid_h)
		right = -( G(i + 1, j, 1)*grid_h + G(i + 1, j + 1, 1)*grid_h)/(2*grid_h)
		bottom = -( G(i, j, 1)*grid_h + G(i + 1, j, 1)*grid_h)/(2*grid_h)
		top = -( G(i, j + 1, 1)*grid_h + G(i + 1, j + 1, 1)*grid_h)/(2*grid_h)
		loss = (G(i, j, 2)*grid_h*grid_h + G(i + 1, j, 2)*grid_h*grid_h + G(i + 1, j + 1, 2)*grid_h*grid_h + G(i, j + 1, 2)*grid_h*grid_h)/4
		center = loss - (left + right + bottom + top)

		!Assigning Values
		A(k, k) = center
		A(k - 1, k) = left
		A(k + 1, k) = right
		A(k - n, k) = bottom
		A(k + n, k) = top
	END SUBROUTINE general_conditions




	SUBROUTINE indices(i,j,k,n)
		!gets indices of geometry from iterate: 'k' and number of cells: 'n'
		IMPLICIT NONE
		INTEGER, INTENT(IN) :: k, n
		INTEGER, INTENT(INOUT) :: i, j
		REAL :: z
		z = ((k - 1)/ n)
		j = FLOOR(z)
		i = MOD(k, n) - 1
		IF (i < 0) THEN
			i = n - 1
		END IF
	END SUBROUTINE indices

END MODULE matrix_mod