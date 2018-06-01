module mult
	implicit none
contains 
	subroutine mm(mx1, mx2, result, status)
		implicit none
		real (kind = 8), intent(in) :: mx1(:,:) ! mx1 matrix
		real (kind = 8), intent(in) :: mx2(: ,:) ! mx2 matrix
		real (kind = 8), intent(out) :: result(:,:) ! result matrix
		integer (kind = 4), intent(out) :: status ! 1 if error, 0 if ok
		integer :: mx1_s(2)	
		integer :: mx2_s(2)	
		integer :: i,j,k,ichunk,i_c,j_c
		integer :: sum

		ichunk=1024
		mx1_s = shape(mx1)	
		mx2_s = shape(mx2)
		
		!check status
		if (mx1_s(1) /= mx2_s(2)) then	
            result = 0	
			status = 1
		else if (DOT == 0 .and. CACHE == 0) then !normal
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
		else if (DOT == 1 .and. CACHE == 0) then !ddot
			status = 0
			do i = 1,mx1_s(2)     
				do j = 1,mx2_s(1)
					result(j,i)=dot_product(mx1(:,i),mx2(j,:))
				end do
			end do
		else if(DOT == 0 .and. CACHE == 1) then !cache
			status = 0
			do i_c = 1, mx1_s(2), ichunk
				do j_c = 1, mx2_s(1), ichunk
					do i = i_c,min(i_c + ichunk - 1, mx1_s(2))   
						do k = j_c, min(j_c + ichunk - 1, mx2_s(1))
							do j = 1,mx1_s(1)
								result(k,i) = result(k,i) + mx1(j,i) * mx2(k,j)
							end do
						end do
					end do
				end do
			end do 
		else if(DOT == 1 .and. CACHE == 1) then !both
			do i_c = 1, mx1_s(2), ichunk
				do j_c = 1, mx2_s(1), ichunk
					do i = i_c,min(i_c + ichunk - 1, mx1_s(2))   
						do k = j_c, min(j_c + ichunk - 1, mx2_s(1))
							result(k,i)=dot_product(mx1(:,i),mx2(k,:))
						end do
					end do
				end do
			end do
		end if
		

	end subroutine mm
end module mult
