program prog_6b
    implicit none 
    
    real*8 ::  an1 , an2 
    integer :: n , j
    write(*,*) 'Which term of the fibonacci-series do you want? '
    read(*,*) n 
    
    an1 =  fib(n) 
    an2 = binform(n) 
    write(*,*) ' The n-th element of the Fibonacci-series is: '
    write(*,'(f100.5)') an1 

    write(*,*) ' The n-th element of the Fibonacci-series is: '
    write(*,'(f100.5)') an2     
    
    write(*,*) ' The n-th element relative difference of the Fibonacci-series is: '
    write(*,'(f100.5)') an1 - an2     
    
    write(*,*) 'test fib(n)'
    
    do j = 1, n 
        write(*,*) fib(j)
        
    enddo 
contains 
    function fib(n) 
        implicit none 
        
        ! input arguments 
        integer, intent(in) :: n 
        
        ! local variables 
        real*8 :: fibn = 0d0 
        real*8 :: fib1 = 1d0, fib2 = 1d0 
        integer :: j 
        
        ! function value 
        real*8 :: fib 
        
        if (n == 1 .or. n == 2) then 
            fibn = 1d0 
        elseif (n>=3) then 
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
