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

  ! Allocate memory dynamically.
  allocate(arr(n), stat=i)
  if(i /= 0) then
    print *, "Error allocating memory."
    stop
  end if

  ! Populate array.  Input validation included.
  print *, "Enter the array elements:"
  do i = 1, n
    read *, arr(i)
    if (arr(i) < 0) then
      print *, "Error: Array elements must be non-negative."
      deallocate(arr)
      stop
    end if
  end do

  ! Calculate sum using a loop.
  sum = 0
  do i = 1, n
    sum = sum + arr(i)
  end do

  ! Print the sum.
  print *, "Sum of array elements:", sum

  ! Deallocate memory.
  deallocate(arr)

end program good_code


program bad_code
  implicit none
  integer :: arr(100), n, i, sum

  ! No input validation.
  print *, "Enter array size:"
  read *, n

  ! No dynamic memory allocation; fixed size array, potential buffer overflow.
  print *, "Enter array elements:"
  do i = 1, n
    read *, arr(i)
  end do

  ! No error handling for out-of-bounds access.
  sum = 0
  do i = 1, n
    sum = sum + arr(i)
  end do

  print *, "Sum:", sum

end program bad_code
