MODULE version_data
	IMPLICIT NONE
	CHARACTER :: Version= "0.1", Title="Transport Solver", Authors="Uday Mehta | Alexandre Chong"
	CHARACTER :: date, time, zone
	INTEGER :: values

CONTAINS
	INTEGER FUNCTION ver_status(Version, Title, Authors)
		WRITE(*,*) Version, Title, Authors
		DATE_AND_TIME(date,time,zone,values)
		WRITE(*,*) date, times, zone, values

END MODULE version_data