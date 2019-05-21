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
    ! Utility function 
    real*8, parameter :: alpha(2) = (/0.3d0, 0.4d0/)
    ! Production function
    real*8, parameter :: beta(2) = (/ 0.3d0, 0.6d0/)
    ! Shares
    real*8, parameter :: a0(2) = (/0.2d0, 0.2d0/)
    real*8, parameter :: a(2, 2) = reshape((/ 0d0, 0.3d0, 0.2d0, 0d0 /),(/2,2/))
    real*8, parameter :: ID(2, 2) = reshape((/ 1d0, 0d0, 0d0, 1d0 /),(/2,2/))
    ! Government 
    real*8, parameter :: G = 3d0
    real*8            :: tauw = 0d0
    real*8            :: taur = 0d0
    real*8            :: tauc(2) = 0d0
    real*8            :: Ybarn, q(2), p(2), w, wn, r, rn
    real*8            :: Xd(2), Y(2), ky(2), ly(2), K(2), L(2), ell

contains

    ! function to determine market equilibrium
    function markets(x)

        use toolbox

        implicit none
        real*8, intent(in) :: x(:)
        real*8 :: markets(size(x, 1))

        ! copy producer prices and taxes
        w       = x(1)
        tauc(1) = x(2)
        tauc(2) = tauc(1)
        
        r       = 1d0
        q(2)    = r 

        ! 1. calkulate K/Y and L/Y
        ky = a0*((1d0-beta)/beta*w/r)**beta
        ly = a0*(beta/(1d0-beta)*r/w)**(1d0-beta)

        ! 2. determine producer prices
        q(1) = (w*ly(1)+r*ky(1) + a(2,1)*q(2))*(1d0/(1d0-a(1,1)))
        ! call lu_solve(ID-transpose(a), q)
        ! 3. consumer prices and demands
        p  = q*(1d0+tauc)
        wn = w*(1d0-tauw)
        rn = r*(1d0-taur)
        Ybarn = wn*Tbar+rn*Kbar
        Xd = alpha/p*Ybarn
        ell  = (1d0-alpha(1)-alpha(2))/wn*Ybarn

        ! 4. determine output levels
        Y(1) = Xd(1)+G
        Y(2) = Xd(2)
        call lu_solve(ID-a, Y)

        ! 5. compute K and L
        K = ky*Y
        L = ly*Y

        ! 6. check markets and budget
        markets(1) = L(1)+L(2)-Tbar + ell
        markets(2) = (1d0-a(1,1))*Y(1) - a(1,2)*Y(2) - Xd(1) - G

    end function

end module

