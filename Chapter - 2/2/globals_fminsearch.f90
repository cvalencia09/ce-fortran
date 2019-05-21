module globals2

    implicit none 
    real*8 :: gamma = 0.5d0
    real*8 :: beta= 1d0 
    real*8 :: r = 0d0 
    real*8 :: w = 1d0
    
contains 
    function util(c) 
    
        implicit none 
        real*8, intent(in) :: c
        real*8 :: util
        
        util = -((w-c/(1d0+r))**(1d0-1d0/gamma)/(1d0-1d0/gamma) + beta*c**(1d0-1d0/gamma)/(1d0-1d0/gamma))
         
        
    end  function util 
end module 
