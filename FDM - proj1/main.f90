program main
    	use gaussian_elimination
    implicit none
    integer :: precise, n, length, i, j
    character(10) :: value
    real (kind = prec), allocatable :: matrix(:, :), vector(:)
    real (kind = prec)              :: h
    real(kind = 16) :: epsilon

    call get_command_argument(1, value)
    read(value , *) n
	
    h = 1./n

    allocate(matrix(n,n))
    allocate(vector(n))
    matrix(:, :) = 0
    vector(:) = 0
    vector(n) = 1
    
    do i = 1,n-1
    	matrix(i,i+1) = 1 / (h * h)
    end do
    do i = 2,n
    	matrix(i,i-1) = 1 / (h * h)
    end do
    do i=1,n
	matrix(i,i) = -2 / (h * h)
    end do	
    
    call gauss(matrix,vector,n)

    epsilon = 0
    do i = 1,n
        epsilon = epsilon + abs(vector(i) - real(i)/real(n))
    end do
    
    print *, epsilon/(n-2)
    print *,vector


    deallocate(matrix)
    deallocate(vector)



end program main
