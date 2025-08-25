program good_bad_fortran
  implicit none
  integer, allocatable :: good_array(:), bad_array(:)
  integer :: i, n = 1000

  ! Good Code: Dynamic memory allocation and error handling.
  allocate(good_array(n), stat=i)
  if (i /= 0) then
    print *, "Allocation failed for good_array"
    stop
  end if
  good_array = [(i, i=1, n)]

  ! Bad Code:  No error handling for allocation, potential memory leak.
  allocate(bad_array(n)) ! No error checking
  bad_array = [(i*2, i=1,n)]

  ! Good Code: Deallocate memory after use.
  deallocate(good_array)

  ! Bad Code: Memory leak - bad_array not deallocated.

  print *, "Good array sum:", sum(good_array) !Should print 0 after deallocation.

  ! Clean exit
  end program good_bad_fortran
