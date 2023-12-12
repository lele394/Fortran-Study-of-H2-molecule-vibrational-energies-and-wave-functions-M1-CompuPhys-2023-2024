program q3
   implicit none
   
   character(32) :: arg ! commandline argument holder
   character(32) :: in_name = "idkjustputtingstuffhere"

   integer :: length ! n° of line in file

   real, dimension(:), allocatable :: position, psi, phi

   integer :: unit, iostat, i

   real :: sum=0
   real :: psi_square=0

   real :: B = 60.80 ! defined in subject

   call get_command_argument(1, arg)
   read(arg, *) in_name

   !  counts number of line and allocates accordingly
   call count_lines(in_name, length)
   allocate(position(0:length), psi(0:length), phi(0:length))

   ! populate arrays ????
   open(unit=unit, file=in_name, status='old', action='read', iostat=iostat)
   do i = 1, length
      read(unit, *) position(i), psi(i)
   end do
   close(unit)


   psi_square = 0
   do i=1, length
      psi_square = psi_square + (psi(i)**2)
   end do

   
   do i=1, length
      phi(i) = psi(i)/sqrt(psi_square)
   end do

   sum = 0
   do i = 1, length
      sum = sum + phi(i)**2/position(i)**2
   end do

   print *, "resulting sum", sum
   print *, "new B for this file is ", sum*B
   print *, sum*B















   contains
   subroutine count_lines(filename, num_lines)
      character(len=*), intent(in) :: filename
      integer, intent(out) :: num_lines
      integer :: CL_unit, CL_iostat
      character(len=100) :: line
    
      ! Initialize the count
      num_lines = 0
    
      ! Open the file
      open(unit=CL_unit, file=filename, status='old', action='read', iostat=CL_iostat)
    
      ! Count the lines
      do
        read(CL_unit, *, iostat=CL_iostat) line
        if (CL_iostat /= 0) exit
        num_lines = num_lines + 1
      end do
    
      ! Close the file
      close(unit, iostat=iostat)
    
    end subroutine count_lines

    
end program q3