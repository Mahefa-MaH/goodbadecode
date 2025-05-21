program good_code
  implicit none
  integer, allocatable :: arr(:)
  integer :: n, i, sum

  ! Get the size of the array from the user.
  print *, "Enter the size of the array:"
  read *, n

  ! Allocate memory for the array.
  allocate(arr(n))

  ! Read the array elements from the user.  Error handling included.
  print *, "Enter the array elements:"
  do i = 1, n
    read *, arr(i)
    if( .not. .allocated(arr) ) then
       print *, "Allocation failed. Exiting."
       stop
    end if
  enddo

  ! Calculate the sum of the array elements.
  sum = 0
  do i = 1, n
    sum = sum + arr(i)
  enddo

  ! Print the sum of the array elements.
  print *, "The sum of the array elements is:", sum

  ! Deallocate the array.  Good practice for memory management.
  deallocate(arr)

end program good_code


program bad_code
  implicit none
  integer :: arr(100), n, i, sum

  ! Get the size of the array from the user. No error handling.
  print *, "Enter the size of the array:"
  read *, n

  ! Read the array elements from the user.  No bounds checking.
  print *, "Enter the array elements:"
  do i = 1, n
    read *, arr(i)
  enddo

  ! Calculate the sum of the array elements.
  sum = 0
  do i = 1, n
    sum = sum + arr(i)
  enddo

  ! Print the sum of the array elements.
  print *, "The sum of the array elements is:", sum

end program bad_code
