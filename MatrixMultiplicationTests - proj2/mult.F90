module mult
	implicit none
contains 
	subroutine mm(mx1, mx2, result, status)
		implicit none
		real (kind = 8), intent(in) :: mx1(:,:) ! first matrix
		real (kind = 8), intent(in) :: mx2(: ,:) ! second matrix
		real (kind = 8), intent(out) :: result(:,:) ! result matrix
		integer (kind = 4), intent(out) :: status ! 1 if error, 0 if ok
		integer :: mx1_s(2)	
		integer :: mx2_s(2)	
		integer :: i,j,k
		integer :: sum


		mx1_s = shape(mx1)	
        mx2_s = shape(mx2)

		!check status
		if (mx1_s(1) /= mx2_s(2)) then	
            result = 0	
			status = 1
		else 
			status = 0
			do i = 1,mx1_s(2)  	
                do j = 1,mx2_s(1)
                    sum = 0	
                    do k = 1,mx1_s(1)	
                        sum = sum + mx1(k,i) * mx2(j,k)	
                    end do	
                    result(j,i) = sum	
                end do	
            end do	
		end if
		

	end subroutine mm
end module mult
