module equations

    implicit none 
    
contains 
    function monopoly(p) 
    
        implicit none 
        real*8, intent(in) :: p(:)
        real*8 :: monopoly(size(p,1)) 
        real*8 :: c = 0.1d0
        
        monopoly(1) = (10d0 - 2d0*p(1)) + (-0.5d0*p(2)) - c
        monopoly(2) = (20d0 - 2d0*p(2)-0.5d0*p(1)) - c
    end  function monopoly 
end module 
