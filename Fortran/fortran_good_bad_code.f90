program good_code
  implicit none
  integer, allocatable :: arr(:)
  integer :: n, i, sum, err
  
  ! Get array size from user.  Error handling included.
  print *, "Enter the size of the array:"
  read(*,*,iostat=err) n
  if (err /= 0) then
    print *, "Invalid input. Exiting."
    stop
  end if

  ! Allocate array dynamically.
  allocate(arr(n), stat=err)
  if (err /= 0) then
    print *, "Memory allocation failed. Exiting."
    stop
  end if

  ! Populate array.  Handles potential errors during input.
  print *, "Enter the array elements:"
  do i = 1, n
    read(*,*,iostat=err) arr(i)
    if (err /= 0) then
      print *, "Invalid input. Exiting."
      deallocate(arr)
      stop
    end if
  end do

  ! Calculate sum.
  sum = 0
  do i = 1, n
    sum = sum + arr(i)
  end do

  ! Print sum and deallocate array.
  print *, "Sum of array elements:", sum
  deallocate(arr)

end program good_code


program bad_code
  implicit none
  integer :: arr(100), n, i, sum

  print *, "Enter the size of the array:"
  read(*,*) n

  !No check for n exceeding array bounds!
  print *, "Enter the array elements:"
  do i = 1, n
    read(*,*) arr(i)
  end do

  sum = 0
  do i = 1, n
    sum = sum + arr(i)
  end do

  print *, "Sum of array elements:", sum

end program bad_code
