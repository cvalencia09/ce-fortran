program prog_3a
    implicit none 
    
    real*8 :: x, y 
    integer :: j
    
    
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
    
end program 
