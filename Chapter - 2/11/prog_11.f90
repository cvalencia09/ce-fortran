
program simplex_alg

    use toolbox

    implicit none
    real*8 :: c(12), x(12), A(7, 12), b(7)
    integer :: j

    ! set up matrix, target vector and coefficients
    A(1, :) = (/1d0, 1d0, 1d0, 1d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0  /)
    A(2, :) = (/0d0, 0d0, 0d0, 0d0, 1d0, 1d0, 1d0, 1d0, 0d0, 0d0, 0d0, 0d0  /)
    A(3, :) = (/0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 1d0, 1d0, 1d0, 1d0  /)
    
    A(4, :) = (/-1d0, 0d0, 0d0, 0d0, -1d0, 0d0, 0d0, 0d0, -1d0, 0d0, 0d0, 0d0  /)
    A(5, :) = (/0d0, -1d0, 0d0, 0d0, 0d0, -1d0, 0d0, 0d0, 0d0, -1d0, 0d0, 0d0  /)
    A(6, :) = (/0d0, 0d0, -1d0, 0d0, 0d0, 0d0, -1d0, 0d0, 0d0, 0d0, -1d0, 0d0  /)
    A(7, :) = (/0d0, 0d0, 0d0, -1d0, 0d0, 0d0, 0d0, -1d0, 0d0, 0d0, 0d0, -1d0  /)

        
    
    b(:) = (/11d0, 13d0, 10d0, -5d0, -7d0, -13d0, -6d0 /)

    c(:) = (/10d0, 70d0, 100d0, 80d0, 130d0, 90d0, 120d0, 110d0, 50d0, 30d0, 80d0, 10d0/)

    ! solve linear program
    call solve_lin(x, c, A, b, 7, 0, 0)

    ! output
    write(*,'(a/)') 'Solution'
    write(*,'(a)')'                B1        B2        B3        B4'
    write(*,'(/a,4f10.2)')'     A1 = ',(x(j), j=1,4)
    write(*,'(/a,4f10.2)')'     A2 = ',(x(j), j=5,8)
    write(*,'(/a,4f10.2)')'     A3 = ',(x(j), j=9,12)
    ! write(*,'(/a,f10.2)')' Cons 1 = ', b(1)-sum(A(1, :)*x)
    ! write(*,'(a,f10.2)')' Cons 2 = ', b(2)-sum(A(2, :)*x)
    ! write(*,'(a,f10.2)')' Cons 3 = ', b(3)-sum(A(3, :)*x)

end program
