include "globals_fzero.f90"

program consumer_problem

    
    use equations
    use toolbox
    
    implicit none 
    real*8 :: var(3)
    logical :: check
    
    ! initialize c
    var = 0.1d0 
    
    
    
    ! find root 
    call fzero(var, consumer, check)
    
    !if (check) stop 'Error: fzero did not converge'
    
    ! output 
    write(*, '(/a)') '                  Output'
    write(*, '(a, f10.4)') 'Consumption 1: ', var(1)
    write(*, '(a, f10.4)') 'Consumption 2: ', var(2)
    write(*, '(a, f10.4)') 'Lambda:        ', var(3)
    
    ! Parameters 
    write(*,'(/a)') 'Used parameters: '
    write(*,'(4(4x, a, f7.2/))') 'Gamma',gamma, 'Beta ', beta, 'R    ', r, 'Wage ',w
    
end program 
