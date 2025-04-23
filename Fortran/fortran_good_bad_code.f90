program good_code
  implicit none
  integer, allocatable :: arr(:)
  integer :: n, i, sum
  
  ! Get the array size from the user.
  print *, "Enter the number of elements:"
  read *, n
  
  ! Allocate memory for the array.  Error handling included.
  allocate(arr(n), stat=i)
  if (i /= 0) then
    print *, "Allocation failed!"
    stop
  end if

  ! Read array elements.
  print *, "Enter the elements:"
  read *, (arr(i), i=1, n)

  ! Calculate the sum.
  sum = sum(arr)

  ! Print the sum.
  print *, "Sum:", sum

  ! Deallocate the array.
  deallocate(arr)

end program good_code


program bad_code
  implicit none
  integer :: arr(100), n, i, sum
  
  print *, "Enter the number of elements (max 100):"
  read *, n

  print *, "Enter the elements:"
  read *, (arr(i), i=1, n)

  sum = 0
  do i = 1, n
    sum = sum + arr(i)
  end do

  print *, "Sum:", sum

end program bad_code
