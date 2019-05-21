program prog_9
    use toolbox 
    implicit none 

    ! Variables 
    real*8 :: u(4), c, gamma(4) 
    integer :: j 
    gamma(:) = (/ 0.25d0, 0.5d0, 0.75d0, 1.25d0 /) 
    c = 1d0 
    
    do j = 1, 4 
        u(j) = utility(c, gamma(j)) 
    enddo 
    
    call plot(gamma, u, legend = 'U(c)')
    call execplot(title = 'Changing gamma') 
contains 
    function utility(c, gamma)
        implicit none 

        ! input variables 
        real*8,intent(in) :: c, gamma 
        
        ! local variables 
        real*8 :: a, b, utility  
        
        ! output variables 
        if (c > 0d0) then  
            a = c**(1d0-(1d0/gamma))
            b = (1d0 - (1d0/gamma))
            utility = dble(a)/dble(b) 
        elseif (c <= 0d0 ) then 
            write(*,*) 'You need to put a positive value for c! ' 
            
            utility = 0d0 
            
            stop 
        endif 
    end function  
end program 
