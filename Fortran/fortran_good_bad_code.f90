program good_code
  implicit none
  integer, allocatable :: arr(:)
  integer :: n, i, sum = 0
  ! Get the size of the array from the user.
  print *, "Enter the size of the array:"
  read *, n
  ! Allocate memory for the array.  Error handling included.
  allocate(arr(n), stat=i)
  if (i /= 0) then
    print *, "Memory allocation failed."
    stop
  end if
  ! Get the elements of the array from the user.
  print *, "Enter the elements of the array:"
  read *, (arr(i), i = 1, n)
  ! Calculate the sum of the elements of the array.
  do i = 1, n
    sum = sum + arr(i)
  end do
  ! Print the sum of the elements of the array.
  print *, "The sum of the elements of the array is:", sum
  !Deallocate the array
  deallocate(arr)
end program good_code


program bad_code
  implicit none
  integer :: arr(100), n, i, sum
  ! No input validation or error handling
  print *, "Enter the size of the array (max 100):"
  read *, n
  print *, "Enter the elements of the array:"
  read *, (arr(i), i = 1, n)
  ! Potential buffer overflow if n > 100
  sum = 0
  do i = 1, n
    sum = sum + arr(i)
  enddo
  print *, "The sum is:", sum
end program bad_code
