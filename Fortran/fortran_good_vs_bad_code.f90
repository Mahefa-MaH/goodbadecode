program good_code
  implicit none
  integer, allocatable :: arr(:)
  integer :: n, i, sum

  ! Get the size of the array from the user.
  print *, "Enter the size of the array:"
  read *, n

  ! Allocate memory for the array.
  allocate(arr(n))

  ! Get the elements of the array from the user.
  print *, "Enter the elements of the array:"
  do i = 1, n
    read *, arr(i)
  enddo

  ! Calculate the sum of the elements of the array.
  sum = 0
  do i = 1, n
    sum = sum + arr(i)
  enddo

  ! Print the sum of the elements of the array.
  print *, "The sum of the elements of the array is:", sum

  ! Deallocate the array.
  deallocate(arr)

end program good_code


program bad_code
  implicit none
  integer :: arr(100), n, i, sum

  ! Get the size of the array from the user.  No error handling for n > 100.
  print *, "Enter the size of the array:"
  read *, n

  ! Get the elements of the array from the user. No bounds checking.
  print *, "Enter the elements of the array:"
  do i = 1, n
    read *, arr(i)
  enddo

  ! Calculate the sum of the elements of the array.
  sum = 0
  do i = 1, n
    sum = sum + arr(i)
  enddo

  ! Print the sum of the elements of the array.
  print *, "The sum of the elements of the array is:", sum

end program bad_code
