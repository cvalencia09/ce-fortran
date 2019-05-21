program prog_5
    implicit none 
    
    real*8 ::  an  
    integer :: n 
    write(*,*) 'Which term of the fibonacci-series do you want? '
    read(*,*) n 
    
    an =  fib(n) 
    
    write(*,*) ' The n-th element of the Fibonacci-series is: '
    write(*,'(f20.5)') an 
    
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
            do j = 1, n - 2  
                fibn = fib1 + fib2
                fib2 = fib1  
                fib1 = fibn 
            enddo 
        endif
        
        fib = fibn
         
    end function 
end program 
