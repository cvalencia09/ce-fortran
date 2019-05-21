include "globals_fminsearch.f90"

program consumer_problem2

    
    use globals2
    use toolbox
    
    implicit none 
    real*8 :: c, f, a, b
    
    ! bounds
    a = 0d0 
    b = w*(1d0+r)
    
    ! set starting point 
    c = (a+b)/2d0 
    
    ! find root 
    call fminsearch(c, f, a, b, util)
        
    ! output 
    write(*, '(/a)') '                  Output'
    write(*, '(a, f10.4)') 'Consumption 1: ', w-c/(1d0+r)
    write(*, '(a, f10.4)') 'Consumption 2: ', c
    write(*, '(a, f10.4)') 'Utility:        ', -f
    
    ! Parameters 
    write(*,'(/a)') 'Used parameters: '
    write(*,'(4(4x, a, f7.2/))') 'Gamma',gamma, 'Beta ', beta, 'R    ', r, 'Wage ',w
    
end program 
