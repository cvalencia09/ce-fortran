program ReadNumbers 
    ! declaration of variables
    implicit none 
    integer :: x, y, temp 
    
    ! executable code 
    write(*,*) 'Type a real value for variable x' 
    read(*,*) x
    
    write(*,*) 'Type a real value for variable y' 
    read(*,*) y
    
    temp = x+y 
    write(*, '(3i4)') x, y, temp 
    temp = x-y 
    write(*, '(3i4)') x, y, temp 
    temp = x*y 
    write(*, '(3i4)') x, y, temp 
    temp = x/y 
    write(*, '(3i4)') x, y, temp 
end program 
