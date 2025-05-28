program good_code
  implicit none
  integer, allocatable :: arr(:)
  integer :: n, i, sum = 0

  ! Get input from user.  Robust error handling included.
  print *, "Enter the number of elements:"
  read(*,*,iostat=i) n
  if (i /= 0) then
    print *, "Invalid input. Exiting."
    stop
  end if

  allocate(arr(n),stat=i)
  if (i /= 0) then
    print *, "Memory allocation failed. Exiting."
    stop
  end if

  print *, "Enter the elements:"
  read(*,*) (arr(i), i=1,n)


  !Efficient sum calculation using a do loop.
  do i = 1, n
    sum = sum + arr(i)
  end do

  print *, "Sum:", sum

  deallocate(arr)

end program good_code


program bad_code
  implicit none
  integer :: arr(100),n,i,sum
  print *, "Enter the number of elements:"
  read(*,*) n

  print *, "Enter the elements:"
  read(*,*) (arr(i),i=1,n)

  sum = 0
  do i=1,n
    sum = sum + arr(i)
  enddo

  print *, "Sum:",sum

end program bad_code
