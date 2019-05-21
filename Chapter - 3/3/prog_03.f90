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
! #VC# VERSION: 1.0  (18 Mayo 2019)
!
!##############################################################################
include "prog_03m.f90"

program market4

    use globals
    use toolbox

    implicit none
    real*8 :: x, U, C
    logical :: check

    ! initial guess
    x = 0.2d0

    ! find market equilibrium
    call fzero(x, markets, check)

    ! check whether fzero converged
    if(check)then
        write(*,'(a/)')'Error in fzero !!!'
        stop
    endif

    
    ! get utility level
    C = (1d0-alphau)*Ybarn/((P_index**v)*omega)
    U =((1d0-alphau**(1d0/v))*C**mu +((alphau)**(1d0/v))*ell**mu )**(1d0/mu)
    
    call print_market(Xd, Y, C, L, K, p, Tbar, ell, Kbar, U, P_index, omega)
    
contains 
    subroutine print_market(Xd, Y, C, L, K, p, Tbar, ell, Kbar, U, P_index, omega)
        implicit none 
        real*8, intent(in) :: Xd(:), Y(:), C, L(:), K(:), p(:), Tbar, ell, Kbar, U, P_index, omega
        ! variables 
        ! output
        write(*,'(/a)')'GOODS MARKET 1 :'
        write(*,'(4(a,f6.2,2x))')' X1 =',Xd(1),' Y1 =',Y(1), &
        ' q1 =',p(1),' p1 =',p(1)

        write(*,'(/a)')'GOODS MARKET 2 :'
        write(*,'(4(a,f6.2,2x))')' X2 =',Xd(2),' Y2 =',Y(2), &
        ' q2 =',p(2),' p2 =',p(2)
        write(*,'(/a)')'Consumption Bundle :'
        write(*,'(1(a,f6.2,2x))') 'C = ', C

        write(*,'(/a)')'LABOR MARKET :'
        write(*,'(4(a,f6.2,2x))')' L1 =',L(1),' L2 =',L(2),' L  =',Tbar - ell, &
        ' w  =',w

        write(*,'(/a)')'CAPITAL MARKET :'
        write(*,'(4(a,f6.2,2x))')' K1 =',K(1),' K2 =',K(2),' K  =',Kbar, &
        ' r  =',r

        write(*,'(/a)')'UTILITY :'
        write(*,'(a,f6.2,2x)')' U  =',U
    
        write(*,'(/a)')'Leissure :'
        write(*,'(a,f6.2,2x)')' l  =',ell
    
        write(*,'(/a)')'Consumer Price Index :'
        write(*,'(a,f6.2,2x)')' P  =',P_index
    
        write(*,'(/a)')'Price Index :'
        write(*,'(a,f6.2,2x)')' Omega  =',omega
    end subroutine
end program
