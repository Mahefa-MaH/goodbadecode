program good_code
  implicit none
  integer, allocatable :: array(:)
  integer :: n, i, sum

  ! Get array size from user
  print *, "Enter the size of the array:"
  read *, n

  ! Allocate memory for the array
  allocate(array(1:n), stat=i)
  if (i /= 0) then
    print *, "Memory allocation failed."
    stop
  end if

  ! Read array elements from user
  print *, "Enter the array elements:"
  read *, (array(i), i=1,n)

  ! Calculate the sum of array elements
  sum = 0
  do i = 1, n
    sum = sum + array(i)
  end do

  ! Print the sum
  print *, "The sum of the array elements is:", sum

  ! Deallocate the array
  deallocate(array)

end program good_code


program bad_code
  implicit none
  integer :: array(100), n, i, sum

  ! No error handling for potential array bounds issues
  print *, "Enter the number of elements (max 100):"
  read *, n

  print *, "Enter the array elements:"
  read *, (array(i), i=1,n)

  sum = 0
  do i = 1, n
    sum = sum + array(i)
  end do

  print *, "The sum is:", sum

end program bad_code
