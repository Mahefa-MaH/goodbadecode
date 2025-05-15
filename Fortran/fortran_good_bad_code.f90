program good_code
  implicit none
  integer, allocatable :: arr(:)
  integer :: n, i, sum
  
  ! Get input
  print *, "Enter the size of the array:"
  read *, n
  
  ! Allocate memory
  allocate(arr(n))
  
  ! Input array elements
  print *, "Enter the array elements:"
  read *, (arr(i), i=1,n)
  
  ! Calculate sum using a loop for better readability and maintainability.
  sum = 0
  do i = 1, n
    sum = sum + arr(i)
  end do

  ! Deallocate memory
  deallocate(arr)

  ! Output the sum
  print *, "Sum of array elements:", sum

end program good_code


program bad_code
  implicit none
  integer :: arr(100), n, i, sum
  
  print *, "Enter array size (max 100):"
  read *, n

  print *, "Enter array elements:"
  read *, (arr(i), i=1,n)

  sum = 0
  i = 1
  sum = sum + arr(i)
  i = 2
  sum = sum + arr(i)
  i = 3
  sum = sum + arr(i)
  ! ... and so on, this is highly inefficient and unmaintainable.  
  
  print *, "Sum:", sum

end program bad_code
