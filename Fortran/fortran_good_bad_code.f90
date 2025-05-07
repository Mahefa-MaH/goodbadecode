program good_code
  implicit none
  integer, allocatable :: array(:)
  integer :: i, n, sum

  ! Get array size from user.  Error handling included.
  print *, "Enter the size of the array:"
  read *, n
  if (n <= 0) then
    print *, "Error: Array size must be positive."
    stop
  end if

  ! Allocate array dynamically.
  allocate(array(1:n), stat=i)
  if (i /= 0) then
    print *, "Error allocating memory."
    stop
  end if

  ! Populate and sum the array.
  print *, "Enter the array elements:"
  read *, (array(i), i=1, n)
  sum = sum(array)

  ! Print the sum.
  print *, "The sum of the array elements is:", sum

  ! Deallocate the array to prevent memory leaks.
  deallocate(array)

end program good_code


program bad_code
  implicit none
  integer :: array(100), i, sum

  ! No error handling for array bounds or input.
  print *, "Enter 100 array elements:"
  read *, (array(i), i=1, 100)
  sum = 0
  do i = 1, 100
    sum = sum + array(i)
  end do

  print *, "The sum is:", sum

end program bad_code
