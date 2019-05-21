module globals 
    
    implicit none 
    real*8, parameter :: gamma = 0.5d0
    real*8, parameter :: beta= 0d0
    real*8, parameter :: r = 0d0 
    real*8, parameter :: w1 = 1d0, w2 = 10d0
    
contains 
    function utility(c)
        implicit none 
        ! variable in 
        real*8, intent(in) :: c(:) 
        
        ! local variables 
        real*8 :: u1, u2, u3 , coef, c1, rate, budget 
        real*8 :: c_lim(size(c,1))
        logical :: check

        ! variable out 
        real*8 :: utility
        
        ! some definitions 
        coef  = 1d0-1d0/gamma
        rate = 1d0+r
        budget = w1 + w2/rate
        c_lim(1) = max(c(1), 1d-10)
        c_lim(2) = max(c(2), 1d-10)
        
        c1 =  max(budget - (c_lim(1)/rate + c_lim(2)/(rate**2d0)),1d-10)
        u1 = (c1**coef)/coef
        u2 = (c_lim(1)**coef)/coef
        u3 = (c_lim(2)**coef)/coef

        utility = -(u1 + u2*beta + u3*beta**2d0) 
                
        if (budget - (c_lim(1)/rate + c_lim(2)/(rate**2d0)) + c(1)/rate + c(2)/(rate**2d0) /= budget ) then 
            utility = 100d0
        else 
            utility = utility  + 1000d0*abs(c_lim(1)-c(1)) + 1000d0*abs(c_lim(2)-c(2))
        endif  
        
    end function utility
end module 
