!##############################################################################
! MODULE globals
!
! This code is published under the GNU General Public License v3
!                         (https://www.gnu.org/licenses/gpl-3.0.en.html)
!
! Authors: Cristian Valencia
!          cristian.valencia09@gmail.com
!
! #VC# VERSION: 1.0  (05 Mayo 2019)
!
!##############################################################################
module globals

    implicit none
    real*8, parameter :: Kbar = 10d0
    real*8, parameter :: Tbar = 30d0
    real*8, parameter :: alpha = 0.3d0
    real*8, parameter :: alpha1 = 0.3d0
    real*8, parameter :: alpha2 = 0.4d0
    real*8, parameter :: v = 1.5d0
    real*8, parameter :: vx = 0.5d0

    real*8, parameter :: mu = 1d0 - v
    real*8, parameter :: mux = 1d0 - vx
    real*8, parameter :: beta(2) = (/ 0.3d0, 0.6d0/)

contains


    ! function to determine market equilibrium
    function markets(x)

        implicit none
        real*8, intent(in) :: x(:)
        real*8 :: markets(size(x, 1))
        real*8 :: Ybar, p(2), w, r, P_index, omega, Yd

        ! copy prices
        p(1) = 1d0
        p(2) = x(1)
        w    = x(2)
        r    = x(3)
        P_index = alpha1*p(1)**(1d0-vx) + alpha2*p(2)**(1d0-vx)
        omega = (1d0-alpha)*P_index**(1d0-v) + alpha*w**(1d0-v)
       

        ! calculate total income
        Ybar = w*Tbar+r*Kbar
        Yd = Ybar - w*alpha*Ybar/((w**v)*omega)
        ! get market equations
        markets(1) = 1d0/p(1)-(beta(1)/w)**beta(1)*((1d0-beta(1))/r)**(1d0-beta(1))
        markets(2) = 1d0/p(2)-(beta(2)/w)**beta(2)*((1d0-beta(2))/r)**(1d0-beta(2))
        markets(3) = beta(1)*alpha1*yd*(p(1)**(1d0-vx))/(w*P_index)+beta(2)*alpha2*Yd*(p(2)**(1d0-vx))/(w*P_index) & 
                     + alpha*Ybar/((w**v)*omega)-Tbar

    end function

end module

