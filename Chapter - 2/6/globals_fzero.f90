module equations

    implicit none 
    real*8 :: marketprice = 3d0
    
contains 
    function price_0(d) 
    
        implicit none 
        real*8, intent(in) :: d
        real*8 :: price_0
        price_0 = 4d0/((d+1d0)**2d0) - marketprice
        end  function price_0 
end module 
