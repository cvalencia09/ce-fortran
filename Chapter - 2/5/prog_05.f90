program integration_problem
    
    use toolbox
    implicit none 
    integer, parameter :: n = 100 
    real*8, parameter :: a = -1d0, b = 1d0 
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
    f(:,1) = exp(-x(:,1))
    
    do i = 0, (n-1)
        f(i,2) = exp(-(x(i,1)+x(i+1,1))/2d0)
        ! write(*, '(2f12.7)') f(i,2) , w(i,3)
    enddo
    f(n,2) = 0d0
    
    ! output numerical and analytical solution 
    write(*,'(a, f7.3,a,f7.3,a)') 'f(x) = exp(-x) over the interval [',a,',',b,']'
    write(*,'(a,f10.6)') 'Analytical Solution: ', -exp(-b) + exp(-a) 
    write(*,'(/a)') 'Numerical Solutions:  '
    write(*,'(3x,a,f10.6)') 'Trapezoide:     ', sum(w(:,1)*f(:,1), 1)
    write(*,'(3x,a,f10.6)') 'Simpson:        ', sum(w(:,2)*f(:,1), 1)+ sum(w(:,3)*f(:,2), 1)
    write(*,'(3x,a,f10.6)') 'Gauss-Legendre: ', sum(w(:, 4)*f(:,1),1)
    
    
    
    ! calculate function valuas at nodes 

    f(:,1) = abs(x(:,1))**0.5d0
    
    do i = 0, (n-1)
        f(i,2) = abs((x(i,1)+x(i+1,1))/2d0)**0.5d0
        ! write(*, '(2f12.7)') f(i,2) , w(i,3)
    enddo
    f(n,2) = 0d0
    
    
    write(*,'(/a, f7.3,a,f7.3,a)') 'f(x) = |x|**0.5d0 over the interval [',a,',',b,']'
    write(*,'(a,f10.6)') 'Analytical Solution: ', 2d0*(b)**(3d0/2d0)*2d0/3d0
    write(*,'(/a)') 'Numerical Solutions:  '
    write(*,'(3x,a,f10.6)') 'Trapezoide:     ', sum(w(:,1)*f(:,1), 1)
    write(*,'(3x,a,f10.6)') 'Simpson:        ', sum(w(:,2)*f(:,1), 1)+ sum(w(:,3)*f(:,2), 1)
    write(*,'(3x,a,f10.6)') 'Gauss-Legendre: ', sum(w(:, 4)*f(:,1),1)
end program 
