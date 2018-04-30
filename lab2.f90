

function f(x) result (y)
    implicit none
    real (kind = 8) :: x,y
    y=x**3+5*x**2-10*x+7
end function f




program main
    implicit none
    real (kind = 8) :: a,b,x,cal = 0
    integer (kind = 4) :: i
    real,allocatable :: tab(:)
    real (kind = 8) :: f
    integer, parameter :: SIZE=512

    allocate(tab(SIZE))



    a=0
    b=10
    x = (b-a)/SIZE



    do i=1,SIZE
        a = a + x
        tab(i) = f(a)
    end do

    !tab=tab**2

    do i=1,(SIZE -1)
        cal=cal+0.5*x*(tab(i)+tab(i+1))
    end do

    print *, cal

end program main