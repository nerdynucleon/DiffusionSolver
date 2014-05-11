MODULE input
	IMPLICIT NONE
CONTAINS

	SUBROUTINE error_check1(n, n_mat)
		IMPLICIT NONE
		INTEGER, INTENT(IN) :: n, n_mat

		PRINT *, "error checking..."

		IF (n < 3) THEN
			STOP "the number of cells must be greater than or equal to three"
		END IF
		IF (n_mat < 1) THEN
			STOP "must have at least one material"
		END IF
	END SUBROUTINE error_check1

	SUBROUTINE error_check2(grid_h, T, R, L, B, iter, tol, converge,source_type)
		IMPLICIT NONE
		REAL, INTENT(IN) :: grid_h, tol
		INTEGER, INTENT(IN) :: iter
		CHARACTER, INTENT(IN) :: T, R, L, B, converge, source_type

		IF (.NOT. positive(grid_h)) THEN
			STOP "grid spacing not positive real number"
		END IF
		IF (.NOT. positive_int(iter)) THEN
			STOP "iter not positive integer"
		END IF
		IF (.NOT. positive(tol)) THEN
			STOP "tolerance not positive real number"
		END IF
		IF ((source_type /= "F") .AND. (source_type /= "Q")) THEN
			STOP "invalid source type select F or Q"
		END IF
		IF ((converge /= "R") .AND. (source_type /= "A")) THEN
			STOP "invalid convergence condition select R or A"
		END IF
		IF ((T /= "V") .AND. (T /= "R")) THEN
			STOP "invalid top boundary condition select V or R"
		END IF
		IF ((T /= "V") .AND. (T /= "R")) THEN
			STOP "invalid top boundary condition select V or R"
		END IF
		IF ((R /= "V") .AND. (R /= "R")) THEN
			STOP "invalid right boundary condition select V or R"
		END IF
		IF ((B /= "V") .AND. (B /= "R")) THEN
			STOP "invalid bottom boundary condition select V or R"
		END IF
		IF ((L /= "V") .AND. (L /= "R")) THEN
			STOP "invalid left boundary condition select V or R"
		END IF

		PRINT *, "error checking complete! all tests passed"
	END SUBROUTINE error_check2

	LOGICAL FUNCTION positive(x)
		IMPLICIT NONE
		REAL :: x
		IF (ABS(x) == x) THEN
			positive = .TRUE.
		ELSE
			positive = .FALSE.
		END IF
	END FUNCTION positive

	LOGICAL FUNCTION positive_int(x)
		IMPLICIT NONE
		INTEGER :: x
		IF (ABS(x) == x) THEN
			positive_int = .TRUE.
		ELSE
			positive_int = .FALSE.
		END IF
	END FUNCTION positive_int

END MODULE input