TransportSolver
===============

2D Fortran Transport Solver NE 155 Sp2014 UC Berkeley

Project Proposal:	NE 155  | Spring 2014
=======================================
Team Members: 		Uday Mehta | Alexandre Chong

Goal: Develop a 2D transport solver in Fortran. The solver should be capable of solving non-homogeneous materials given a continuous function or an array of discrete values the grid will have uniform spacing. The boundary conditions will be top and right reflecting and vacuum at the bottom and left.

Major Steps to Execute 
* Data acquisition:						
	- Accept inputs from user:
	- Solution method
	- Max number of iterations or max time
	- Grid spacing
	- Constants / functions
	- Output format
* Error check user input
* Matrix Generation:
	- Building the matrix
* Matrix Solving:
	- Iterative Methods
	* SOR
	* jacobi
	* Gauss-Seidel
* Matrix Inversion
	- x = A-1b
* Displaying Output:
	- Vector solution
	- Graphical solution

Deadlines
- April 11		Familiarize ourselves with the nature, constraints, and applications of the problem
- April 14		Come up with a framework for implementation (bare outline for the code)
- April 16		Complete code for transport solver
- April 18		Test and debug code to ensure success
- April 19		First draft of written report
- April 21		Final version of written report
- April 28		First draft of presentation
- May 2		Final version of presentation and run-through
- May 5		First draft of final written report
- May 8		Final version of final written report

Division of Labor: 
Alex will take on the majority of sub-points 1 and 3 of the execution steps, while Uday will aid as necessary. Uday will also take on sub-points 2 and 4 of the execution steps and write the majority of the written reports, while Alex will aid as necessary.
