program good_code
  implicit none
  integer, allocatable :: arr(:)
  integer :: n, i, sum
  
  print *, "Enter the size of the array:"
  read *, n
  
  allocate(arr(n))
  
  print *, "Enter the elements of the array:"
  read *, (arr(i), i=1,n)
  
  sum = 0
  do i = 1, n
    sum = sum + arr(i)
  enddo
  
  print *, "Sum of array elements:", sum
  deallocate(arr)
end program good_code


program bad_code
  implicit none
  integer :: arr(100), n, i, sum
  
  print *, "Enter the size of the array (max 100):"
  read *, n
  
  print *, "Enter the elements of the array:"
  read *, (arr(i), i=1,n)
  
  sum = 0
  do i = 1, 100 
    sum = sum + arr(i)
  enddo
  
  print *, "Sum of array elements:", sum
end program bad_code
