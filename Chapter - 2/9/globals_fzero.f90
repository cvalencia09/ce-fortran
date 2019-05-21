module reaction

    implicit none 
    real*8, parameter :: alpha = 1d0, eta = 1.5d0
    integer, parameter :: m = 100
    real* 8 :: p
    
contains 
    function reaction_cournot(q) 
    
        implicit none 
        real*8, intent(in) :: q(:)
        real*8 :: reaction_cournot(size(q,1)) 
        integer :: i 
                
        do i = 1, m
            reaction_cournot(i) = p + q(i)*(1d0/(-eta*(p**(-eta-1d0))))  &
                            - (alpha*sqrt(q(i))+q(i)**2d0)
        enddo
    end  function reaction_cournot
end module 
