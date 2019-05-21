include "module_globals.f90"

program min_problem
    
    use globals 
    use toolbox
    
    implicit none 
    real*8 :: c(2), f, a(2), b(2), temp1 , temp2 , consumption
    
    temp1 = (w1 + w2/(1d0+r))*(1d0+r)
    temp2 = (w1 + w2/(1d0+r))*((1d0+r)**2)
    ! bounds

    ! b = w1 + w2/(1d0+r)
    b(1) = temp1 
    b(2) = temp2
    a = 0d0 

    ! set starting point 
    c = (a+b)/2d0
    
    ! Initial values and guess 
    write(*,'(/a)') 'Used values: '
    write(*,'(6(4x, a, f8.4/))') 'a1 ',a(1), 'a2 ', a(2), 'b1 ', b(1), 'b2 ',b(2), 'c1 ', c(1), 'c2 ', c(2) 
    
    ! Parameters 
    write(*,'(/a)') 'Used parameters: '
    write(*,'(5(4x, a, f7.2/))') 'Gamma ',gamma, 'Beta  ', beta, 'R     ', r, 'Wage 1',w1, 'Wage 2', w2 
    
    ! Budget
    write(*,'(a, f12.7 )') 'Budget Constraint: ', w1 + w2/(1d0+r)
    ! find root 
    call fminsearch(c, f, a, b, utility)
        
    ! output 
    write(*,'(/a)') 'RESULTS'
    write(*, '(/a)') '                  Output'
    write(*, '(3x, a, f10.4)') 'Consumption 1: ', w1 + w2/(1d0+r)-(c(1)/(1d0+r) + c(2)/((1d0+r)**2d0))
    write(*, '(3x, a, f10.4)') 'Consumption 2: ', c(1)
    write(*, '(3x, a, f10.4)') 'Consumption 3: ', c(2)
    write(*, '(3x, a, f10.4)') 'Utility:       ', -f
    
    consumption = w1 + w2/(1d0+r)-(c(1)/(1d0+r) + c(2)/((1d0+r)**2d0)) + c(1)/(1d0+r) + c(2)/((1d0+r)**2d0)
    write(*,'(/a)') 'is the constraint OK?'
    write(*,'(3x,a,f12.8)') 'Consumption over time: ', consumption
    if (consumption == w1 + w2/(1d0+r)) then 
        write(*,'(/a)') 'It is Okay! '
    else 
        write(*,'(/a)') 'It is not Okay! '
    endif 
    
 
 

end program 
