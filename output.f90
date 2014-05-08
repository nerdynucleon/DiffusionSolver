MODULE output


CONTAINS


	SUBROUTINE repeat_input1(n, n_mat, n_squares, n_circles)
		IMPLICIT NONE
		INTEGER, INTENT(IN) :: n, n_mat, n_squares, n_circles
		WRITE(2,*) "Number Cells, Number of Materials"
		WRITE(2,*) n, n_mat
		WRITE(2,*) "Number of Squares, Number of Circles"
		WRITE(2,*) n_squares, n_circles
	END SUBROUTINE repeat_input1

	SUBROUTINE repeat_input2(squares, circles, grid_h, D, sig_a, source_mat, T, R, L, B, iter, tol, converge, source_type)
		INTEGER, INTENT(IN) ::  iter
		REAL, INTENT(IN) :: tol
		REAL, ALLOCATABLE, DIMENSION(:), INTENT(IN) :: D, sig_a, source_mat
		REAL, ALLOCATABLE, DIMENSION(:,:), INTENT(IN) :: squares, circles
		character(1), INTENT(IN) :: source_type, converge, T, R, L, B
		WRITE(2,*) "Squares: material, x1, x2, y1, y2, priority"
		WRITE(2,*) squares
		WRITE(2,*) "Circles: material, x, y, priority"
		WRITE(2,*) circles
		WRITE(2,*) "Grid Spacing"
		WRITE(2,*) grid_h
		WRITE(2,*) "Diffusion Constants"
		WRITE(2,*) D
		WRITE(2,*) "Absorption Cross Sections"
		WRITE(2,*) sig_a
		WRITE(2,*) "Source Values"
		WRITE(2,*) source_mat
		WRITE(2,*) "Boundary Conditions: T,R,L,B"
		WRITE(2,*) T,R,L,B
		WRITE(2,*) "Iteration Properties: maximum iterations, tolerance, convergence type"
		WRITE(2,*) iter
		WRITE(2,*) tol
		WRITE(2,*) converge
		WRITE(2,*) "Source Type"
		WRITE(2,*) source_type
	END SUBROUTINE repeat_input2

	SUBROUTINE geometry_out(G)
		IMPLICIT NONE
		REAL, DIMENSION(:,:,:), INTENT(IN) :: G
		WRITE(2,*) "Geometry"
		WRITE(2,*) "See Geometry.csv for geometry"
		WRITE(3,'(*(G0.10,:,","))') G
	END SUBROUTINE geometry_out

	SUBROUTINE matrix_out(A)
		IMPLICIT NONE
		REAL, DIMENSION(:,:), INTENT(IN) :: A
		WRITE(2,*) "matrix"
		WRITE(2,*) "See matrix.csv for matrix"
		WRITE(4,'(*(G0.10,:,","))') A
	END SUBROUTINE matrix_out

	SUBROUTINE solution_out(x)
		REAL, DIMENSION(:), INTENT(IN) :: x
		WRITE(2,*) "flux:"
		WRITE(2,*) "See results.csv for solution"
		WRITE(5,'(*(G0.10,:,","))') x
	END SUBROUTINE solution_out


END MODULE output