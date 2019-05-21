!##############################################################################
! MODULE globals
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
    ! Endowment 
    real*8, parameter :: Kbar = 10d0
    real*8, parameter :: Tbar = 30d0
    
    ! Production function
    real*8, parameter :: sigma = 0.5d0 
    real*8, parameter :: rho = 1d0 - 1d0/sigma
    
    ! Utility function and indexes 
    real*8, parameter :: alphau = 0.3d0
    real*8, parameter :: alpha(2) = (/0.3d0, 0.4d0/)
    real*8, parameter :: beta(2) = (/ 0.3d0, 0.6d0/)
    
    real*8, parameter :: v = 1.5d0
    real*8, parameter :: vx = 0.5d0

    real*8, parameter :: mu = 1d0 - v
    real*8, parameter :: mux = 1d0 - vx
    
    ! For the zero-profit condition of the firms 
    real*8, parameter :: a(2, 2) = reshape((/ 0d0, 0d0, 0d0, 0d0 /),(/2,2/))
    real*8, parameter :: ID(2, 2) = reshape((/ 1d0, 0d0, 0d0, 1d0 /),(/2,2/))

    ! Government 
    real*8, parameter :: G = 3d0
    real*8            :: tauw = 0d0
    real*8            :: taur = 0.43d0
    real*8            :: tauc(2) = 0d0
    
    ! Variables 
    real*8            :: Ybarn, Yd, q(2), p(2), w, wn, r, rn, P_index, omega
    real*8            :: Xd(2), Y(2), ky(2), ly(2), K(2), L(2), ell

contains

    ! function to determine market equilibrium
    function markets(x)

        use toolbox

        implicit none
        real*8, intent(in) :: x(:)
        real*8 :: markets(size(x, 1))

        ! copy producer prices and taxes
        w       = 1d0
        r       = x(1)
        tauc(1) = x(2)
        tauc(2) = tauc(1)
        
        ! 1. calkulate K/Y and L/Y
        ly = (beta+(1d0-beta)*((beta*r/((1d0-beta)*w))**(1d0-sigma)))**(-1d0/rho)
        ky = ((1d0-beta)+beta*(((1d0-beta)*w/(beta*r))**(1d0-sigma)))**(-1d0/rho)

        ! 2. determine producer prices
        q = w*ly+r*ky    
        call lu_solve(ID-transpose(a), q)

        ! 3. consumer prices
        p  = q*(1d0+tauc)
        wn = w*(1d0-tauw)
        rn = r*(1d0-taur)
        
        ! Prince indexes
        P_index = alpha(1)*p(1)**(1d0-vx) + alpha(2)*p(2)**(1d0-vx)
        omega = (1d0-alphau)*P_index**(1d0-v) + alphau*w**(1d0-v)
        
        ! Income and leisure
        Ybarn = wn*Tbar+rn*Kbar
        ell = alphau*Ybarn/((wn**v)*omega)
        Yd = Ybarn - wn*ell
        
        ! Demands 
        Xd(1) = alpha(1)*Yd/((p(1)**v)*P_index)
        Xd(2) = alpha(2)*Yd/((p(2)**v)*P_index)

        
        ! 4. determine output levels
        Y(1) = Xd(1)
        Y(2) = Xd(2)
        call lu_solve(ID-a, Y)

        ! 5. compute K and L
        K = ky*Y
        L = ly*Y

        ! 6. check markets and budget
        markets(1) = K(1)+K(2)-Kbar
        markets(2) = q(1)*G-sum(tauc*q*Xd)-tauw*w*(Tbar-ell)-taur*r*Kbar

    end function

end module

