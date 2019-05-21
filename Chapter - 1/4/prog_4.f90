program prog_4
    implicit none 
    
    real*8 :: x, y, z 
    logical :: lvalue 
    write(*,*) 'Value for x: '
    read(*,*) x 

    write(*,*) 'Value for y: '
    read(*,*) y     

    write(*,*) 'Value for z: '
    read(*,*) z
    
    write(*,*) 'Evaluating logical expression... '
    
    lvalue = x >= 3 .and. y <= 4 .and. z == 5 .or. x <= y .and. y < z
    write(*,*) lvalue 
    
end program 
