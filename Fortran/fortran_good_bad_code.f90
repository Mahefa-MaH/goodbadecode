program good_code
  implicit none
  integer, allocatable :: array(:)
  integer :: n, i, sum
  
  ! Get the size of the array from the user.
  print *, "Enter the size of the array:"
  read *, n
  
  ! Allocate memory for the array.  Error handling included.
  allocate(array(1:n), stat=i)
  if (i /= 0) then
    print *, "Memory allocation failed."
    stop
  end if

  ! Fill the array with values.
  print *, "Enter the array elements:"
  read *, (array(i), i=1,n)

  ! Calculate the sum of the array elements.
  sum = 0
  do i = 1, n
    sum = sum + array(i)
  end do

  ! Print the sum.
  print *, "The sum of the array elements is:", sum

  ! Deallocate the array.
  deallocate(array)

end program good_code


program bad_code
  implicit none
  integer :: array(100), n, i, sum

  print *, "Enter the size of the array:"
  read *, n

  ! No error handling for array bounds.  Vulnerable to buffer overflow.
  print *, "Enter the array elements:"
  read *, (array(i), i=1,n)

  sum = 0
  do i = 1, n
    sum = sum + array(i)
  end do

  print *, "The sum is:", sum

end program bad_code
