PROGRAM Transport_Solver
	USE version_data
	open(unit=1, file="TransportInput", iostat=ios, status="old", action="read")
	if ( ios /= 0 ) stop "Error opening file name"
	open(unit=2, file="TransportOutput", iostat=ios, status="new", action="write")
	if ( ios /= 0 ) stop "Error opening file name"
	
	


	WRITE(*,*) Version,Title,Authors
	CALL date_and_time(start_date,start_time,zone)
	WRITE(*,*) 'Date: ', start_date
	WRITE(*,*) 'Time: ', start_time
	WRITE(*,*) 'Timezone: ', zone




	CALL date_and_time(end_date,end_time,zone)
	WRITE(*,*) 'Date: ', end_date
	WRITE(*,*) 'Time: ', end_time


END PROGRAM Transport_Solver