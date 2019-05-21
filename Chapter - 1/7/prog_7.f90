program prog_7
    implicit none 
    
    real*8 :: x
    integer :: n, iter = 1000000, p, q ,d   
    real*8 :: Dsum(2:12) 

    do n = 1, iter

        call random_number(x)
        p = int(6*x) + 1 
        call random_number(x)
        q = int(6*x) + 1 
        
        d = p + q
        Dsum(d) = Dsum(d) + 1   
    enddo

    write(*,*) 'Dsum! '
    do n = 2, 12
        if (Dsum(n)< 1) then 
            Dsum(n) = 0 
        endif 
        write(*,*) Dsum(n) 

    enddo
    
    write(*,*) 'The probability distribution! '
    do n = 2, 12

        write(*,*) dble(Dsum(n))/dble(iter) 

    enddo
end program 
