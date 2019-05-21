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

    integer :: ii
    real*8 :: a(3), b(3), w(3), f, sigma_2

    ! get returns and their statistical properties
    call returns
    a = (/-1d0, -1d0, -1d0/) 
    b = (/1d0, 1d0, 1d0/) 
    w = 0d0
    
    call fminsearch(w, f, a, b, sigmap2_rf)    
     
    
     ! print results
    write(*,'(a,3f8.3)')'Expected return risky asset',(mu(ii), ii = 1, 3)
    write(*,*)
    write(*,'(a,3f8.3)')'Variance-covariance matrix ',(sig(1,ii), ii = 1, 3)
    write(*,'(a,3f8.3)')'                           ',(sig(2,ii), ii = 1, 3)
    write(*,'(a,3f8.3)')'                           ',(sig(3,ii), ii = 1, 3)
    
    sigma_2 = dot_product(matmul(w, sig), w)
    write(*, '(a)') 'Portfolio Choice : '
    write(*,'(a)') '               Stock1  Stock2  Stock3  Risk-free' 
    if (r_f == 0d0) then 
        write(*, '((a, 5(f6.3,2x)))') 'gamma: ', gamma , 1d0-w(2)-w(3),(w(ii)*100d0, ii = 2, NN), 0d0
    else 
        write(*, '((a, 5(f6.3,2x)))') 'gamma: ', gamma , (w(ii)*100d0, ii = 1, NN), (1d0-sum(w))*100d0
    endif
    write(*, '((/a, f6.3))') 'Sigma2 :', sigma_2
    
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
