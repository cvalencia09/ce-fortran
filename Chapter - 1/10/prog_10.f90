program prog_9
    use toolbox 
    implicit none 

    ! Variables 
    integer, parameter ::  nsize =10
    real*8 :: u(nsize), c, a, b
    integer :: j 
    
    a = 1d0 
    b = 2d0
    
    call utility_int(a, b, u)
    
    write(*,*) ' j ', '       u(cj)' 
    do j = 1, size(u) 
        write(*,'(i3, a, f12.4)') j, '  ', u(j)  
    enddo 
contains 
    function utility(c, gamma)
        implicit none 

        ! input variables 
        real*8,intent(in) :: c, gamma 
        
        ! local variables 
        real*8 :: a, b
        
        ! output variables 
        
        real*8 :: utility 
        
        if (c > 0d0) then  
            a = c**(1d0-(1d0/gamma))
            b = (1d0 - (1d0/gamma))
            utility = dble(a)/dble(b) 
        elseif (c <= 0d0 ) then 
            write(*,*) 'You need to put a non-negative value for c! ' 
            
            utility = 0d0 
            
            stop 
        endif 
    end function
    
    subroutine utility_int(a , b, u) 
        implicit none 
        
        ! input variables 
        real*8, intent(in) :: a, b
        
        ! local variables 
        integer :: j , n 
        real*8 :: c
        real*8 :: gamma = 0.5d0
        
        ! output (or in/out) variables 
        real*8, intent(inout) :: u(:) 
    
        if ((0 < a) .and. (a < b)) then 
            n = size(u)
            do j = 1, n 
                c = a + ((dble(j)-1d0)/(dble(n)-1d0))*(b-a)
                u(j) = utility(dble(c), gamma)
            enddo 
        else 
            write(*,*) 'Error, variable a need to be less than b, and it need to be greater than zero !'
            stop 
        endif 
    end subroutine   
end program 
