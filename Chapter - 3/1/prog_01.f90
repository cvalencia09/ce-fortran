!##############################################################################
! PROGRAM market1
!
! ## The market equilibrium solution to the static general equilibrium model
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
include "prog_01m.f90"

program market1

    use globals
    use toolbox

    implicit none
    real*8 :: x(3), L(2), K(2), Y(2), Ybar, w, r, p(2), U, P_index
    logical :: check

    ! initial guess
    x(:) = 0.5d0

    ! find market equilibrium
    call fzero(x, markets, check)

    ! check whether fzero converged
    if(check)then
        write(*,'(a/)')'Error in fzero !!!'
        stop
    endif

    ! copy prices
    p(1) = 1d0
    p(2) = x(1)
    w    = x(2)
    r    = x(3)

    ! calculate other economic variables
    P_index = alpha*p(1)**(1d0-v) + (1d0-alpha)*p(2)**(1d0-v) 
    Ybar = w*Lbar+r*Kbar
    Y(1) = alpha*Ybar/((p(1)**v)*P_index)
    Y(2) = (1d0-alpha)*Ybar/((p(2)**v)*P_index)
    L    = beta*p*Y/w
    K    = (1d0-beta)*p*Y/r
    U    = ((alpha**(1d0/v))*Y(1)**mu +((1d0-alpha)**(1d0/v))*Y(2)**mu )**(1/mu)

    ! output
    write(*,'(/a)')'GOODS MARKET 1 :'
    write(*,'(4(a,f6.2,2x))')' X1 =',Y(1),' Y1 =',Y(1), &
        ' q1 =',p(1),' p1 =',p(1)

    write(*,'(/a)')'GOODS MARKET 2 :'
    write(*,'(4(a,f6.2,2x))')' X2 =',Y(2),' Y2 =',Y(2), &
        ' q2 =',p(2),' p2 =',p(2)

    write(*,'(/a)')'LABOR MARKET :'
    write(*,'(4(a,f6.2,2x))')' L1 =',L(1),' L2 =',L(2),' L  =',Lbar, &
        ' w  =',w

    write(*,'(/a)')'CAPITAL MARKET :'
    write(*,'(4(a,f6.2,2x))')' K1 =',K(1),' K2 =',K(2),' K  =',Kbar, &
        ' r  =',r

    write(*,'(/a)')'UTILITY :'
    write(*,'(a,f6.2,2x)')' U  =',U
    
    write(*,'(/a)')'Price Index :'
    write(*,'(a,f6.2,2x)')' P  =',P_index

end program
