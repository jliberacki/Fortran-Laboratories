program measure
    use mult
    implicit none
    real(kind = 8), allocatable :: mx1(:,:) 
    real(kind = 8), allocatable :: mx2(:,:) 
    real(kind = 8), allocatable :: result(:,:)
    integer :: status 
    integer (kind = 8) :: N
    character (len=16) :: args
    real :: start, stop

    call get_command_argument(1, args)
    read(args(1:len_trim(args)),'(i8)') N
	
    allocate(mx1(N,N))
    allocate(mx2(N,N))
    allocate(result(N,N))

    mx1 = 1
    mx2 = 2

    if (USENORMAL==0) then
        call cpu_time(start)
        call mm(mx1, mx2, result, status)
        call cpu_time(stop)
        write(*,*) stop-start
    else 
        call cpu_time(start)
        result = matmul(mx1,mx2)
        call cpu_time(stop)
        write(*,*) stop-start
    end if

    deallocate(mx1)
    deallocate(mx2)
    deallocate(result)	
end program measure