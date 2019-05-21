program prog_3c
    implicit none 
    
    real*8 :: x, y, a, b, c, d
    a = 0.000000000003d0 
    b = 0.000000000003
    
    c = 3.1415926535d0
    d = 3.1415926535
     
    x  = 10**9 
    
    write(*,'(f20.2)') x 
    x = 10d0**9 
    
    write(*,'(f20.2)') x 
    x = 10**9d0
    
    write(*,'(f20.2)') x 
    
    y  = 10**10
    
    write(*,'(f20.2)') y
    y = 10d0**10 
    
    write(*,'(f20.2)') y
    y = 10**10d0
    
    write(*,'(f20.2)') y
    
    write (*, '(f30.25)') a 
    
    write (*, '(f30.25)') b 

    write (*, '(f15.12)') c
    
    write (*, '(f15.12)') d     
end program 
