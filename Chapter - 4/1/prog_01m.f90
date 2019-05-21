!##############################################################################
! MODULE globals 
!
!
! This code is published under the GNU General Public License v3
!                         (https://www.gnu.org/licenses/gpl-3.0.en.html)
!
! Authors: Cristian Valencia
!          cristian.valencia09@gmail.com
!
! #VC# VERSION: 1.0  (19 Mayo 2019)
!
!##############################################################################
module globals 
    implicit none 
    integer, parameter :: TT = 5        ! number of time periods
    integer, parameter :: NN = 3        ! number of stocks
    real*8, parameter :: gamma = 10d0   ! risk aversion
    real*8, parameter :: r_f = 0.05d0     ! the risk free rate
    
    real*8 :: omega(NN)
    real*8 :: mu(NN), sig(NN,NN), ID(NN)
    real*8 :: mu_p

contains  
    function sigmap2(w) 
    implicit none 
    
    ! variables in 
    real*8, intent(in) :: w
    
    ! local variables
    real*8 :: w_lim
    real*8 :: weights(3) 
    ! out variables
    real*8 :: sigmap2
    
    
    weights(3) = w
    weights(2) = ((mu_p - mu(1)) - weights(3)*(mu(3) - mu(1))) / (mu(2) - mu(1))
    weights(1) = 1d0- ((mu_p - mu(1)) - weights(3)*(mu(3) - mu(1))) / (mu(2) - mu(1)) -  weights(3)
    ! w1 + w2 + w3 = 1 and mu1*w1 + mu2*w2 + mu3*w3 = mup
    ! w1 = 1 - w2 - w3 and also mu1*(1-w2-w3) + mu2*w2 + mu3*w3 = mup 
    ! then, mu1 + w2*(mu2 - mu1) + w3*(mu3- mu1) = mup 
    ! thus, w2 = ((mup - mu1) - w3*(mu3 - mu1)) / (mu2 - mu1)
    ! also, w1 = 1 - ((mup - mu1) - w3*(mu3 - mu1)) / (mu2 - mu1) - w3 
    sigmap2 =  dot_product(matmul(weights, sig), weights)
    
    end function sigmap2 
    
    function sigmap2_rf(w)
    implicit none 
    
    ! variables in 
    real*8, intent(in) :: w(:)
    ! local variables
    real*8 :: weights(3)
    ! out variables 
    real*8 sigmap2_rf
    
    
    weights(3) = w(3)
    weights(2) = w(2)
   
    
    if (r_f == 0d0) then 
        weights(1) = 1d0-weights(2) - weights(3)
        sigmap2_rf = -(dot_product(weights, mu)-(gamma/2d0)*dot_product(matmul(weights, sig), weights))
    else 
        weights(1) = w(1)
        sigmap2_rf = -(dot_product(weights, mu - r_f)-(gamma/2d0)*dot_product(matmul(weights, sig), weights))
    endif
    
    end function sigmap2_rf 
      
end module  
