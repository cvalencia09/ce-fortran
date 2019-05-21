include "globals_fzero.f90"
program Newspaper_monopoly
    use toolbox
    use equations
    implicit none 
    
    integer, parameter :: nplot = 100, nx = 4, ny = 4 
    real*8, parameter :: x_l = 0.5d0, z_l = 0.5d0, x_r = 12.5d0, z_r = 12.5d0
    integer :: i, j, ppoint(2)
    real*8 :: PR(0:3), PA(0:3), G(0:(nx-1), 0:(ny-1)), G_plot(0:nplot, 0:nplot)
    real*8 :: coeff_G(0:(nx+2-1), 0:(ny+2-1)), pa_grid(0:nplot) ,pr_grid(0:nplot)
    real*8 :: p(1:2)
    logical :: check
    
    G(:,0) = (/ 11.5d0, 70.9d0,  98.3d0,  93.7d0  /)
    G(:,1) = (/ 31.1d0, 82.5d0, 101.9d0, 89.3d0 /)
    G(:,2) = (/ 18.7d0, 62.1d0,  73.5d0,  52.9d0  /)
    G(:,3) = (/-25.7d0,  9.7d0,  13.1d0, -15.5d0 /)
    
    write(*, '(a)') 'G(:,:)= '
    write(*,'(4(f7.1))') (G(:,i), i= 0,3)
    
    call grid_Cons_Equi(PR, 0.5d0, 12.5d0)
    call grid_Cons_Equi(PA, 0.5d0, 12.5d0)
    
    !get the coefficients
    call spline_interp(G, coeff_G)
    write(*, '(a)') 'coeff_G(:,:)= '
    write(*,'(4(f7.1))') (coeff_G(:,i), i= 0,5)
    ! interpolation
    call grid_Cons_Equi(pr_grid, 0.5d0, 12.5d0)
    call grid_Cons_Equi(pa_grid, 0.5d0, 12.5d0)
    do i = 0, nplot 
        do j = 0, nplot
           G_plot(i, j) = spline_eval( (/pr_grid(i), pa_grid(j)/), &
                           coeff_G, (/x_l, z_l/), (/x_r, z_r/))
        enddo
    enddo
    
    ! call plot3d(pr_grid, pa_grid, G_plot)
    
    ppoint = maxloc(G_plot)
    write(*,'(i3)') (ppoint(i), i= 1,2)
    ! write the maximum value to the screen
    write(*,'(a,f15.10)')'Maximum value:   ', &
                                maxval(G_plot)
    write(*,'(a, 2f7.2)')'Where is located? (PR,PA) = ', &
                                pr_grid(ppoint(1)), pa_grid(ppoint(2))
    
    p = 1d0
    ! find root 
    call fzero(p, monopoly, check)
    
    write(*,'(a, 2f7.2)')'Where is located the optimun? (PR,PA) = ', &
                                p(2), p(1)
    write(*,'(a,2f15.10)')'Maximum Error Spline:     ', &
                               abs(p(2)- pr_grid(ppoint(1))), abs(p(1)- pa_grid(ppoint(2)))
end program 
