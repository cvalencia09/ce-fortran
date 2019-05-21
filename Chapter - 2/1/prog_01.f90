program prog_01
    
    use toolbox 
    
    implicit none 
    integer :: j, i 
    real*8 :: A(4,4)
    real*8 :: U(4,4), L(4,4), b(4)  
    
    A(1,:)= (/1d0 ,5d0, 2d0, 3d0 /) 
    A(2,:)= (/1d0, 6d0, 8d0, 6d0 /)
    A(3,:)= (/1d0, 6d0, 11d0, 2d0 /)
    A(4,:)= (/1d0, 7d0, 17d0, 4d0 /)
    
    b = (/1d0, 2d0, 1d0, 1d0 /)
    
    ! decompose matrix 
    call lu_dec(A, L, U)
    
    ! resolving the system 
    call lu_solve(A, b) 
    
    
    ! output
    write(*, '(a, 4f7.2/)') '      x = ', (b(j), j=1,4)
    write(*, '(a,4f7.2/, 2(5x, 4f7.2/))') &
    ' L = ' , ((L(i,j), j = 1,4), i = 1,4)
    write(*, '(a,4f7.2/, 2(5x, 4f7.2/))')  &
    ' U = ' , ((U(i,j), j = 1,4), i = 1,4) 
end program 
