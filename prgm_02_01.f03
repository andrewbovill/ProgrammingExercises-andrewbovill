      Program prgm_02_01

!
!     This program evaluates the Kinetic energy matrix elements for a particle-in-a-box (1D) eigenstates.
!

implicit none
	real :: m,l
  integer :: n1,n2
	real :: PIB_1D_T_Element
  character(len=256)::arg
!
!     Obtain the command line arguments for mass,length,first eigen state, second eigen state.
!

      call GET_COMMAND_ARGUMENT(1,arg)
      read (arg,*) m
      call GET_COMMAND_ARGUMENT(2,arg)
      read (arg,*) l
      call GET_COMMAND_ARGUMENT(3,arg)
      read (arg,*) n1
      call GET_COMMAND_ARGUMENT(4,arg)
      read (arg,*) n2

1000 format(1X,'Kinetic energy matrix element ', I5,','I5,' is ',F12.5,'.')
      write(*,1000) n1, n2, PIB_1D_T_Element(m,l,n1,n2)
      
      End Program prgm_02_01

!   
!     PIB_1D_T_Element function. 
!


      real function PIB_1D_T_Element(m, l, n1, n2)
        implicit none
        real::pi,m,l
        real ::mass, length
        integer :: n1,n2
      
!
!     Define pi
!
      pi =float(4)*atan(float(1))
!
!     Calculate the Kinetic energy matrix element.
!
      if (n1.eq.n2) then
        PIB_1D_T_Element=pi**2*n1**2/(2*m*l**2)
      else if (n1.ne.n2) then
        PIB_1D_T_Element = 0
      endif
      return 

      end function PIB_1D_T_Element
