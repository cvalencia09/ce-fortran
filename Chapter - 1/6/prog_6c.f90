program prog_6c

    use toolbox
    implicit none 
    
    integer :: n   
    integer, parameter :: plotmax = 50
    
    real*8 ::  diff(1:plotmax), x(1:plotmax)

    write(*,'(a,i3)') 'We are going to plot the relative difference between fib amd binform from a = 1 to ', plotmax     
    do n = 1, plotmax 
        x(n) = n  
        
        diff(n) = fib(n)  - binform(n)  
        
    enddo 
    
    call plot(x, diff, legend = 'Relative difference')
    call execplot(title = 'Comparison')       
contains 
    function fib(n) 
        implicit none 
        
        ! input arguments 
        integer, intent(in) :: n 
        
        ! local variables 
        real*8 :: fibn = 0 
        real*8 :: fib1 = 1d0, fib2 = 1d0 
        integer :: j 
        
        ! function value 
        real*8 :: fib 
        
        if (n == 1 .or. n == 2) then 
            fibn = 1d0 
        else 
            fib1 = 1d0 
            fib2 = 1d0 
            do j = 1, n - 2  
                fibn = fib1 + fib2
                fib2 = fib1  
                fib1 = fibn 
            enddo 
        endif
        
        fib = fibn
         
    end function 
    
    function binform(n) 
        implicit none 
        
        ! input arguments 
        integer, intent(in) :: n 
        
        ! local variables 
        real*8 :: fibn  
        real*8 :: psi = (1d0 + sqrt(5d0))/2d0 
        integer :: j 
        
        ! function value 
        real*8 :: binform
        
        binform = (psi**n-(1-psi)**n)/sqrt(5d0)      
    
    end function 
end program 
