PROGRAM Diffusion_Solver
	USE version_data
	USE input
	USE iterate
	USE output
	open(unit=1, file="DiffusionInput", iostat=ios, status="old", action="read")
	if ( ios /= 0 ) stop "Error opening file name"
	open(unit=2, file="DiffusionOutput", iostat=ios, status="new", action="write")
	if ( ios /= 0 ) stop "Error opening file name"

! Display Program Information	
	CALL information()
! Input Error Check
!	CALL input()
!	CALL error_check()
! Build and Iterate
!	CALL matrix()
	CALL iterate()
! Output
!	CALL output()
	CALL done()

END PROGRAM Diffusion_Solver