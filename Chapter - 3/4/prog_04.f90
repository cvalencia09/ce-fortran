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
! #VC# VERSION: 1.0  (19 Mayo 2019)
!
!##############################################################################
include "prog_04m.f90"

program market5

    use globals
    use toolbox

    implicit none
    real*8 :: x(2), U, C
    logical :: check

    ! initial guess
    x(:) = (/0.2d0, 0d0/)

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
    
    call print_market
    call print_table
contains 
    subroutine print_market()
        implicit none 
        ! variables 
        ! output
        write(*,'(/a)')'GOODS MARKET 1 :'
        write(*,'(3(a,f6.2,2x))') ' X1 =',Xd(1),' G  =',G,' Y1 =',Y(1)
        write(*,'(4(a,f6.2,2x))')' q1 =',q(1),' p1 =',p(1),' tc1=',tauc(1)

        write(*,'(/a)')'GOODS MARKET 2 :'
        write(*,'(3(a,f6.2,2x))')' X2 =',Xd(2),' G  =',0d0,' Y2 =',Y(2)
        write(*,'(4(a,f6.2,2x))')' q2 =',q(2),' p2 =',p(2),' tc2=',tauc(2)

        write(*,'(/a)')'LABOR MARKET :'
        write(*,'(3(a,f6.2,2x))')' L1 =',L(1),' L2 =',L(2),' T-F=',Tbar-ell
        write(*,'(3(a,f6.2,2x))')' w  =',w,' wn =',wn, ' tw =',tauw

        write(*,'(/a)')'CAPITAL MARKET :'
        write(*,'(3(a,f6.2,2x))')' K1 =',K(1),' K2 =',K(2),' K  =',Kbar
        write(*,'(3(a,f6.2,2x))')' r  =',r,' rn =',rn, ' tr =',taur

        write(*,'(/a)')'GOVERNMENT :'
        write(*,'(6(a,f6.2,2x))')' tc1=',tauc(1)*q(1)*Xd(1), &
        ' tc2=',tauc(2)*q(2)*Xd(2),' tw =',tauw*w*(Tbar-ell), &
        ' tr =',taur*r*Kbar,' G  =',q(1)*G

        write(*,'(/a)')'Comsumption Bundle :'
        write(*,'(a,f6.2,2x)')' C  =',C
    

        write(*,'(/a)')'UTILITY :'
        write(*,'(a,f6.2,2x)')' U  =',U
    
        write(*,'(/a)')'Leissure :'
        write(*,'(a,f6.2,2x)')' l  =',ell
    
        write(*,'(/a)')'Consumer Price Index :'
        write(*,'(a,f6.2,2x)')' P  =',P_index
    
        write(*,'(/a)')'Price Index :'
        write(*,'(a,f6.2,2x)')' Omega  =',omega
    end subroutine
    
    subroutine print_table() 
        implicit none
        write(*,'(a)') 'Table: '
        write(*,'(a)') '-------------------------------------------------------------------------------'
        write(*,'(12(a,3x))') 'tau1', 'tau2', 'tauw', 'taur', 'w', 'r', 'q1', 'q2', 'X1', 'X2', 'l', 'U'
        write(*,'(a)') '-------------------------------------------------------------------------------'
        write(*,'(12(f6.2))') tauc(1), tauc(2), tauw, taur, w, r, q(1), q(2), Xd(1), Xd(2), ell, U
    end subroutine 
end program
