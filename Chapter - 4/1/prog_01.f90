!##############################################################################
! PROGRAM 
!
! ## Optimal Porfolio weights without and with a risk-free asset
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
include "prog_01m.f90"

program portfoliochoice

    use globals
    use toolbox

    implicit none
    integer, parameter :: n_grid = 1000
    real*8 :: mu_pt(0:n_grid)

    integer :: ii, jj
    real*8 :: a, b, w, w_values(0:n_grid)  , f, fvalues(0:n_grid) 
    real*8 :: w_2, w_values(0:n_grid)

    ! get returns and their statistical properties
    call returns
    a = 0d0 
    b = 1d0
    w = 0d0
    call grid_Cons_Equi(mu_pt, 0.00d0, 0.15d0)
    
    do ii = 0, n_grid 
        mu_p = mu_pt(ii)
        call fminsearch(w, f, a, b, sigmap2)    
        fvalues(ii) = f 
        w_values(ii) = w
    enddo 
    
     ! print results
    write(*,'(a,3f8.3)')'Expected return risky asset',(mu(ii), ii = 1, 3)
    write(*,*)
    write(*,'(a,3f8.3)')'Variance-covariance matrix ',(sig(1,ii), ii = 1, 3)
    write(*,'(a,3f8.3)')'                           ',(sig(2,ii), ii = 1, 3)
    write(*,'(a,3f8.3)')'                           ',(sig(3,ii), ii = 1, 3)
    
    
    write(*, '(a)') 'Minimum Variance Portfolio: ' 
    write(*, '(2(/a, f6.3, 2x))') 'mu_p :', mu_pt(minloc(fvalues)), 'sigma_2 :', minval(fvalues)
    !write(*,'(a)') 'Portfolio shares:   Stock 1   Stock 2   Stock 3    '
    !w_1 = 1d0-((mu_pt(minloc(fvalues)) - mu(1)) - w_values(minloc(fvalues))*(mu(3) - mu(1))) / (mu(2) - mu(1))-w_values(minloc(fvalues))
    !w_2 = ((mu_pt(minloc(fvalues)) - mu(1)) - w_values(minloc(fvalues))*(mu(3) - mu(1))) / (mu(2) - mu(1))
    !w_3 = w_values(int(jj))
    !write(*,'(f6.3)') w_3
    
    ! write(*,'(a,10x,5f10.4)')'MV Portfolio      ', w_1, w_2, w_3 , mu_pt(minloc(fvalues)), minval(fvalues)
    call plot(fvalues, mu_pt,  legend = 'Efficient Porfolios' )
    call plot((/minval(fvalues)/), (/mu_pt(minloc(fvalues))/), marker=7, markersize=1.5d0, noline=.true., legend='MV Portfolio')
    call plot((/sig(1, 1)/), (/mu(1)/), marker=7, markersize=1.5d0, noline=.true., legend='Stock 1')
    call plot((/sig(2, 2)/), (/mu(2)/), marker=7, markersize=1.5d0, noline=.true., legend='Stock 2')
    call plot((/sig(3, 3)/), (/mu(3)/), marker=7, markersize=1.5d0, noline=.true., legend='Stock 3')

    call execplot(xlabel='Portfolio Variance', ylabel='Portfolio Mean Return')! &
    !            xlim =(/0d0, 0.025d0 /),  ylim =(/0d0, 0.12d0 /))
    
contains
    ! get returns and statictical properties
    subroutine returns

        implicit none
        integer :: ii, ij
        real*8 :: odat(0:TT,NN), r(TT,NN)

        ! set vector of ones
        ID(:) = 1.0d0

        ! initialize price data
        odat(0,:) = (/ 1.00d0, 2.00d0, 3.00d0 /)
        odat(1,:) = (/ 1.02d0, 2.65d0, 2.80d0 /)
        odat(2,:) = (/ 1.17d0, 2.40d0, 4.50d0 /)
        odat(3,:) = (/ 1.08d0, 2.70d0, 4.20d0 /)
        odat(4,:) = (/ 1.16d0, 2.85d0, 3.20d0 /)
        odat(5,:) = (/ 1.26d0, 2.75d0, 4.20d0 /)

        ! compute return per period
        do ii = 1, NN
            do ij = 1, TT
                r(ij, ii) = odat(ij, ii)/odat(ij-1, ii)-1d0
            enddo
        enddo

        ! compute expected value
        do ii = 1, NN
            mu(ii) = sum(r(:, ii))/dble(TT)
        enddo

        ! compute covariance matrix
        do ii = 1, NN
            do ij = 1, NN
                sig(ii, ij) = dot_product(r(:, ii)-mu(ii), r(:, ij)-mu(ij))
            enddo
        enddo
        sig = sig/dble(TT)

    end subroutine
end program
