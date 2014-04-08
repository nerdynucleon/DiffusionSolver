MODULE Input

CONTAINS
	LOGICAL FUNCTION user_input()
		! Accepts Parameters from User
		INTEGER :: solution_method
		INTEGER :: patience
		INTEGER :: num_patience

		WRITE(*,*) 'Solution Method 1:Jacobi 2:Gauss Sidel 3:SOR 4:A Inversion'
		WRITE(*,*) '1,2,3 or 4'
		READ(*,*) solution_method
		IF (solution_method /= 4) THEN
			WRITE(*,*) 'Iteration Limit'
			WRITE(*,*) '1:seconds 2:iteration count'
			READ(*,*) patience
			IF (patience == 1) THEN
				WRITE(*,*) 'seconds:'
			ELSE
				WRITE(*,*) 'iterations:'
			END IF
			READ(*,*) num_patience
		ELSE
		END IF
		user_input = .TRUE.
	END FUNCTION user_input

END MODULE Input