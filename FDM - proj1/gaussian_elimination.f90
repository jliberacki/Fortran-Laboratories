module gaussian_elimination
	implicit none
	
contains
    subroutine gauss (matrix, vector, n)
        implicit none
        integer, intent(in) :: n
        integer (kind = 8) :: i, j
        real(kind = prec) :: c
        real(kind = prec), intent(inout) :: matrix(n, n), vector(n)
        do i = 1,n
            do j = 1,n
                if(i /= j) then
                    c = matrix(i,j)/matrix(i,i)
                    matrix(:,j) = matrix(:,j) - c*matrix(:,i)
                    vector(j) = vector(j) - c*vector(i)
                    vector(i) = vector(i) / matrix(i,i)
                    matrix(:,i) = matrix(:,i)/matrix(i,i)
                end if
            end do
        end do
    end subroutine gauss
end module gaussian_elimination
