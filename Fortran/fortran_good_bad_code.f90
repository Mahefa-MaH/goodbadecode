program good_code
  implicit none
  integer, allocatable :: arr(:)
  integer :: n, i, sum
  
  ! Get array size from user.  Error handling included.
  print *, "Enter the size of the array:"
  read *, n
  if (n <= 0) then
    print *, "Error: Array size must be positive."
    stop
  end if

  ! Allocate array dynamically.
  allocate(arr(n), stat=i)
  if (i /= 0) then
    print *, "Error: Memory allocation failed."
    stop
  end if

  ! Populate array and calculate sum.
  sum = 0
  print *, "Enter the array elements:"
  do i = 1, n
    read *, arr(i)
    sum = sum + arr(i)
  end do

  ! Print the sum.
  print *, "Sum of array elements:", sum

  ! Deallocate array to prevent memory leaks.
  deallocate(arr)

end program good_code


program bad_code
  implicit none
  integer :: arr(100), n, i, sum

  ! No input validation. Assumes array size will always be <= 100.
  print *, "Enter the number of elements (max 100):"
  read *, n

  ! No error handling for read.
  print *, "Enter the array elements:"
  do i = 1, n
    read *, arr(i)
    sum = sum + arr(i)
  enddo

  print *, "Sum:", sum

end program bad_code
