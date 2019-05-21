
program Interpolation_cos
    use toolbox
    implicit none 
    
    integer, parameter :: nplot = 1000, n = 10, eachn = 10
    real*8, parameter :: pi = 3.14159265358d0
    integer :: ix, il, ir,i,j
    real*8 :: yplot(0:nplot), xplot(0:nplot),  yreal(0:nplot)
    real*8 :: yi(0:n), xi(0:n)
    real*8 :: varphi, mi(1:n), ti(1:n), f_hat(0:nplot), coeff(n+3)
    
    ! get equidistan plot nodes and data for interpolation 
    call grid_Cons_Equi(xi, 0d0, 2d0*pi)
    yi = cos(xi)
    
    ! get nodes and data for plotting 
    call grid_Cons_Equi(xplot, 0d0, 2d0*pi)
    yreal = cos(xplot)
    call plot(xplot, yreal, legend = 'Original', color ='#060606')
    
    ! MANUAL LINEAR INTERPOLATION
    ! get the coefficients 
    
    do i = 0,n-1
        mi(i+1) = (yi(i+1) - yi(i))/(xi(i+1) - xi(i) )
        ti(i+1) = yi(i) - mi(i+1)*xi(i)
    enddo 
    
    do j = 0, nplot
        do i = 0, n-1   
            if ((xi(i) <= xplot(j)) .and. (xplot(j) <= xi(i+1))) then 
                f_hat(j) = mi(i+1)*xplot(j) + ti(i+1) 
            endif 
        enddo 
    enddo 
    call plot(xplot, f_hat, legend = 'Manual', color ='#FCFF00')
    
    ! USING THE SUBROUTINE linint_Equi
    ! piecewise linear interpolation 
    do ix = 0, nplot 
        call linint_Equi(xplot(ix), 0d0, 2d0*pi, n, il, ir, varphi)  
        yplot(ix) = varphi*yi(il) + (1d0-varphi)*yi(ir)
    enddo 
    
    call plot(xplot, yplot, legend = 'Piecewise linear', color = '#8A1C04')

    ! piecewise cubic spline interpolation
    call spline_interp(yi, coeff)
    do ix = 0, nplot 
        yplot(ix) = spline_eval(xplot(ix), coeff, 0d0 , 2d0*pi) 
    enddo 
    call plot(xplot, yplot, legend = 'Cubic spline', color = '#47FF00')
    
    ! Executing the plot
    call execplot(title = 'Interpolation: cos(x)', xlim =(/0d0, 2d0*pi /))
contains 

end program 
