module equations

    implicit none 
    real*8 :: gamma = 0.5d0
    real*8 :: beta= 1d0 
    real*8 :: r = 0d0 
    real*8 :: w = 1d0
    
contains 
    function consumer(var) 
    
        implicit none 
        real*8, intent(in) :: var(:)
        real*8 :: consumer(size(var,1)) 
        
        consumer(1) = var(1)**(-1d0/gamma) - var(3) 
        consumer(2) = beta*var(2)**(-1d0/gamma) - var(3)/(1d0+r) 
        consumer(3) = var(1) + var(2)/(1d0+r) - w  
        
    end  function consumer 
end module 
