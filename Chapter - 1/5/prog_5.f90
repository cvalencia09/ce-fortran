program prog_5
    implicit none 
    
    real*8 ::  y, T, T1, difft, diffy  
    
    write(*,*) 'Your income is... ? '
    read(*,*) y 
    
    T = TaxIncome(y) 
    T1 = TaxIncome(y+1d0) 
    difft = T1 - T
    diffy = (y+1d0) -y 
    
    write(*,*) ' Tax burden, T(y) | Average Tax rate, T(y)/y | Marginal tax rate, diff(T(y))'
    write(*,'(3(f20.5, a))') T, '      ', T/y ,'      ', difft/diffy 
    
contains 
    function TaxIncome(y) 
        implicit none 
        
        ! input arguments 
        
        real*8, intent(in) :: y 
        
        ! local variables 
        
        real*8 :: x, z 
        ! function value 
        real*8 :: TaxIncome, T 
        
        
        x = ( y - 8130d0) / 10000d0
        z = (y-13469d0)/10000d0 
    
        if (0d0 <= y .and. y<= 8130d0) then 
            T = 0d0
        elseif (8131d0 <= y .and. y <= 13469d0) then 
            T = (993.70d0*x + 1400d0)*x
        elseif (13470d0 <= y .and. y <= 52881d0) then 
            T = (228.74d0*z + 2397d0)*z + 1014d0 
        elseif (51882d0 <= y .and. y <= 250730) then 
            T = 0.42d0*y - 8196d0 
        else 
            T = 0.45d0*y - 15718d0 
        endif   
        
        TaxIncome = T         
    end function 
end program 
