program good_code
  implicit none
  integer, allocatable :: arr(:)
  integer :: n, i, sum = 0
  
  print *, "Enter the size of the array:"
  read *, n
  
  allocate(arr(n))
  
  print *, "Enter the array elements:"
  read *, (arr(i), i = 1, n)
  
  do i = 1, n
    sum = sum + arr(i)
  end do
  
  print *, "Sum of array elements:", sum
  deallocate(arr)
end program good_code


program bad_code
  implicit none
  integer :: arr(100), n, i, sum = 0

  print *, "Enter the size of the array (max 100):"
  read *, n

  if (n > 100 .or. n <=0) then
    print *, "Invalid array size."
    stop
  end if

  print *, "Enter the array elements:"
  read *, (arr(i), i = 1, n)

  do i = 1, n
    sum = sum + arr(i)
  end do

  print *, "Sum of array elements:", sum
end program bad_code
