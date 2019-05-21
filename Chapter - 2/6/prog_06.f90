include "globals_fzero.f90"

program integration_problem
    use equations
    use toolbox
    implicit none 
    integer, parameter :: n = 100
    real*8 :: a = 0d0, b = 1d0, d = 0.01d0 , dummy(3,2), relative_consumer_surplus(3), consumer_surplus_out(3)
    integer :: i
    logical :: check
    
    
    write(*,'(/a, i3/)')'Number of nodes: ', n
    ! find root of the inverse demand function at price = marketprice = 3
    call fzero(d, price_0, check)
    b = d 
    
    ! calculate consumer surplues at market price = 3 
    call consumer_surplus(a, b, marketprice,consumer_surplus_out) 
    do i = 1,3
        dummy(i,1) = consumer_surplus_out(i)
    enddo
    ! find root of the inverse demand function at price = marketprice = 1
    marketprice = 1d0
    call fzero(d, price_0, check)
    b = d
    
    ! calculate consumer surplues at market price = 1
    write(*,'(/a)')' '
    call consumer_surplus(a, b, marketprice,consumer_surplus_out) 
    do i = 1,3
        dummy(i,2) = consumer_surplus_out(i)
    enddo
    do i = 1,3
        relative_consumer_surplus(i) =  dummy(i,2) - dummy(i,1)
    enddo
    
    write(*,'(/a)') 'Relative consumer surplus: ' 
    write(*,'(3x,a,f10.6)') 'Trapezoide:     ', relative_consumer_surplus(1) 
    write(*,'(3x,a,f10.6)') 'Simpson:        ', relative_consumer_surplus(2)
    write(*,'(3x,a,f10.6)') 'Gauss-Legendre: ', relative_consumer_surplus(3)
contains 
    subroutine consumer_surplus(a, b, marketprice, consumer_surplus_out)
        implicit none 
        real*8,intent(in) :: a, b, marketprice
        real*8, intent(inout) :: consumer_surplus_out(3)
        real*8 :: h, x(0:n,2), w(0:n,4), f(0:n,2)
        integer :: i 

        ! calculate quadrature nodes
        h = (b-a)/dble(n)
        x(:,1) = (/(a + dble(i)*h, i = 0,n) /)
    
        ! get weights for the trapezoide rule  
        w(0,1) = h/2d0
        w(n,1) = h/2d0
        w(1:n-1,1) = h 

        ! get weights for the Simpson rule  
        w(0,2) = h/6d0
        w(n,2) = h/6d0
        w(1:n-1,2) = h / 3d0 
    
        w(n,3) = 0d0
        w(0:n-1,3) = 2d0/3d0 * h 
    
        ! get weights for the Gauss-Legendre rule  
        call legendre(a, b, x(:,2), w(:,4))
    
        ! calculate function valuas at nodes 
        f(:,1) = 4d0/((x(:,1)+1d0)**2d0) - marketprice
    
        do i = 0, (n-1)
            f(i,2) = 4d0/(((x(i,1)+x(i+1,1))/2d0+1d0)**2d0) - marketprice
            ! write(*, '(2f12.7)') f(i,2) , w(i,3)
        enddo
        f(n,2) = 0d0
        
        consumer_surplus_out(1) =sum(w(:,1)*f(:,1), 1) 
        consumer_surplus_out(2) =sum(w(:,2)*f(:,1), 1)+ sum(w(:,3)*f(:,2), 1) 
        consumer_surplus_out(3) =sum(w(:, 4)*f(:,1),1) 
        ! output numerical and analytical solution 
        write(*,'(a, f7.3,a,f7.3,a)') 'p(d) = 4/((d+1)**2) - marketprice over the interval [',a,',',b,']'
        ! write(*,'(a,f10.6)') 'Analytical Solution: ', -exp(-b) + exp(-a) 
        write(*,'(/a)') 'Numerical Solutions:  '
        write(*,'(3x,a,f10.6)') 'Trapezoide:     ', consumer_surplus_out(1) 
        write(*,'(3x,a,f10.6)') 'Simpson:        ', consumer_surplus_out(2)
        write(*,'(3x,a,f10.6)') 'Gauss-Legendre: ', consumer_surplus_out(3)
    end subroutine consumer_surplus
end program 
