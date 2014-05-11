MODULE version_data
	CHARACTER(8) :: start_date
	CHARACTER(10) :: start_time
	CHARACTER(8) :: end_date
	CHARACTER(10) :: end_time
	CHARACTER(10) :: zone
	CHARACTER(12) ::Version= "Version 1.0 "
	CHARACTER(17):: Title="Diffusion Solver "
	CHARACTER(28) ::Authors="Uday Mehta | Alexandre Chong"
CONTAINS
	SUBROUTINE information()
		IMPLICIT NONE
		WRITE(2,*) Version,Title,Authors
		PRINT *, Version,Title,Authors
		CALL date_and_time(start_date,start_time,zone)
		WRITE(2,*) 'Date: ', start_date
		PRINT *, 'Date: ', start_date
		WRITE(2,*) 'Time: ', start_time
		PRINT *, 'Time: ', start_time
		WRITE(2,*) 'Timezone: ', zone
		PRINT *, 'Timezone: ', zone
		PRINT *, 'Running Solver...'
	END SUBROUTINE information
	SUBROUTINE done()
		CALL date_and_time(end_date,end_time)
		WRITE(2,*) 'Date: ', end_date
		WRITE(2,*) 'Time: ', end_time
		PRINT *, 'Date: ', end_date
		PRINT *, 'Time: ', end_time
		PRINT *, 'Done!'
	END SUBROUTINE done
END MODULE version_data