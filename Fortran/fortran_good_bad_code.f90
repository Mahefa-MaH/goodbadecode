program good_code
  implicit none
  integer, allocatable :: array(:)
  integer :: n, i, sum

  ! Get the size of the array from the user.
  print *, "Enter the size of the array:"
  read *, n

  ! Allocate the array dynamically.
  allocate(array(1:n), stat=i)
  if (i /= 0) then
    print *, "Allocation failed!"
    stop
  end if

  ! Get the array elements from the user.
  print *, "Enter the array elements:"
  read *, (array(i), i=1, n)

  ! Calculate the sum of the array elements.
  sum = 0
  do i = 1, n
    sum = sum + array(i)
  end do

  ! Print the sum of the array elements.
  print *, "The sum of the array elements is:", sum

  ! Deallocate the array.
  deallocate(array)

end program good_code


program bad_code
  implicit none
  integer :: array(100), n, i, sum

  ! Get the size of the array from the user.  No error handling for exceeding array bounds.
  print *, "Enter the size of the array:"
  read *, n

  ! Get the array elements from the user.  No error handling for exceeding array bounds.
  print *, "Enter the array elements:"
  read *, (array(i), i=1, n)

  ! Calculate the sum of the array elements.
  sum = 0
  do i = 1, n
    sum = sum + array(i)
  end do

  ! Print the sum of the array elements.
  print *, "The sum of the array elements is:", sum

end program bad_code
