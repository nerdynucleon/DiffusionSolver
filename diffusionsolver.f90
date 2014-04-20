PROGRAM Transport_Solver
	USE version_data
	open(unit=1, file="TransportInput", iostat=ios, status="old", action="read")
	if ( ios /= 0 ) stop "Error opening file name"
	open(unit=2, file="TransportOutput", iostat=ios, status="new", action="write")
	if ( ios /= 0 ) stop "Error opening file name"
	
	


	WRITE(2,*) Version,Title,Authors
	CALL date_and_time(start_date,start_time,zone)
	WRITE(2,*) 'Date: ', start_date
	WRITE(2,*) 'Time: ', start_time
	WRITE(2,*) 'Timezone: ', zone




	CALL date_and_time(end_date,end_time,zone)
	WRITE(2,*) 'Date: ', end_date
	WRITE(2,*) 'Time: ', end_time


END PROGRAM Transport_Solver