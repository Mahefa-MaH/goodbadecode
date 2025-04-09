program good_code
  implicit none
  integer, allocatable :: array(:)
  integer :: i, n, sum
  
  ! Get array size from user.  Error handling included.
  print *, "Enter the size of the array:"
  read(*,*,iostat=i) n
  if (i /= 0) then
    print *, "Invalid input. Exiting."
    stop
  end if

  ! Allocate array dynamically. Error handling included.
  allocate(array(1:n), stat=i)
  if (i /= 0) then
    print *, "Memory allocation failed. Exiting."
    stop
  end if

  ! Populate and sum array.
  print *, "Enter the array elements:"
  do i = 1, n
    read(*,*) array(i)
    sum = sum + array(i)
  end do

  ! Print results.
  print *, "Sum of array elements:", sum

  ! Deallocate array.
  deallocate(array)

end program good_code


program bad_code
  implicit none
  integer :: array(100), i, sum
  
  ! No input validation or error handling.
  print *, "Enter 100 array elements:"
  do i = 1, 100
    read(*,*) array(i)
    sum = sum + array(i)
  enddo

  print *, "Sum of array elements:", sum

end program bad_code
