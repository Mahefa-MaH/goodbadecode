program good_code
  implicit none
  integer, allocatable :: arr(:)
  integer :: n, i, sum

  ! Get the size of the array from the user.
  print *, "Enter the size of the array:"
  read *, n

  ! Allocate memory for the array.  Error handling included.
  allocate(arr(n), stat=i)
  if (i /= 0) then
    print *, "Memory allocation failed."
    stop
  end if

  ! Get the array elements from the user.
  print *, "Enter the array elements:"
  read *, (arr(i), i=1, n)

  ! Calculate the sum of the array elements.
  sum = 0
  do i = 1, n
    sum = sum + arr(i)
  end do

  ! Print the sum of the array elements.
  print *, "The sum of the array elements is:", sum

  ! Deallocate the array.
  deallocate(arr)

end program good_code


program bad_code
  implicit none
  integer :: arr(100), n, i, sum

  ! Get the size of the array from the user.  No error handling.
  print *, "Enter the size of the array:"
  read *, n

  ! Get the array elements from the user.  No bounds checking!
  print *, "Enter the array elements:"
  read *, (arr(i), i=1, n)

  ! Calculate the sum of the array elements.  Potential for buffer overflow.
  sum = 0
  do i = 1, n
    sum = sum + arr(i)
  end do

  ! Print the sum of the array elements.  No output formatting.
  print *, sum

end program bad_code
