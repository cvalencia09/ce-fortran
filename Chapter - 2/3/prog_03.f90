
program min_problem

    use toolbox
    
    implicit none 
    integer,parameter :: n = 999 ! NOTE: you only need 4 subintervals to take down this problem
    ! Also, the program is made to display less or equal to 999 subintervals 
    integer ::  i
    real*8 :: a, b, f, x(n + 1),f1(n)
    
    ! bounds
    a = 0d0 
    b = 5d0
    
    ! A) 
    ! initialize min process
    f = minimize(a, b) 
    ! B) The same but with n intervals  
    
    write(*,'(a)') ' "Global" Minimization Process'
    write(*,'(/a)')'     i          x(i) '  
    do i = 1, n + 1 
        x(i) = a + (dble(i)-1)*(b-a)/(dble(n))
        
    enddo
    
    do i = 1, n 
        f1(i) = minimize(x(i),x(i+1)) 
    enddo 
    
    write(*,'(/a)') 'Used points '
    do i = 1, n+1
        write(*,'(3x, i4, 3x, f12.4)') i, x(i)
    enddo
    
    write(*,'(/a)') 'Solution for each subinterval '
    do i = 1, n 
        write(*, '(a, i3, a ,f12.7, a, 2x, a, f7.4 , a, f7.4, a)') ' f(',i,') = ', f1(i),'  in' , '[',x(i),',',x(i+1),']' 
    enddo 
    
    write(*,'(/a)') 'Solution for the first min process '
    write(*, '(a, f12.7)') ' f(x) = ', f 
    
    
    ! Selecting the minimum 
    write(*, '(a,f12.4,a,i3)')'         min(f1) = ' ,f1(minloc(f1)), ' in ', minloc(f1)
contains 
    function minimize(a, b) 
        implicit none 
        real*8, intent(in) :: a, b 
        real*8 :: minimize 
        integer :: j, i
        real*8 :: x(2) , f(2), omega(2), aa, bb 
        
        aa = a 
        bb = b
        ! start iteration process 
        do j = 1, 100
            ! Calculate x1 and x2 and asociated function values 
            omega(1) = (3d0-sqrt(5d0))/2d0
            omega(2) = (sqrt(5d0)-1d0)/2d0
            
            do i = 1,2 
                x(i) = aa + omega(i)*(bb-aa)
                f(i) = x(i)*cos(x(i)**2d0)
            enddo
            
            write(*, '(i4,2x, f12.10)') j, abs(bb - aa) 
            
            ! check for convergence 
            if (abs(bb-aa) < 1d-6) then 
                minimize = f(1)
                write(*, '(/a, f12.7)') ' x = ', x(1) 
                write(*, '(a, f12.7)') ' f(x) = ', minimize 
            endif 
            
            ! get new values 
            
            if (f(1) < f(2)) then 
                bb = x(2) 
            else 
                aa = x(1) 
            endif 
        enddo 
        
    end function minimize
end program 
