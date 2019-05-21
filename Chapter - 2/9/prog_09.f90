include "globals_fzero.f90"

program cournot_oligopoly
    use toolbox
    use reaction
    implicit none 
    
    ! declaring variables 
    real*8, parameter :: coeff_q = 0d0
    integer, parameter ::  N = 20, nplot = 1000
    real*8 :: p_grid(0:N), q_stotal(0:N), q(1:m) = 0d0, q_i(0:N), d(0:N)
    integer :: i,j
    real*8 :: q_plot(0:nplot), p_plot(0:nplot), d_plot(0:nplot)
    real*8 :: coeff(n+3)
    logical :: check
    
    ! get equidistan plot nodes and data for interpolation 
    call grid_Cons_Equi(p_grid, 0.1d0, 3d0)
    do i = 0, N
        ! find roots 
        p = p_grid(i)
        call fzero(q, reaction_cournot, check)
        q_i(i) = q(1)
        q_stotal(i) = dble(m)*q_i(i)
        d(i) = p_grid(i) **(-eta)
    enddo
    call plot(q_stotal, p_grid, legend = 'qi(p)', color = '#020202', noline = .true.)
    call plot(d, p_grid, legend = 'Demand', noline = .true., marker = 10)
    
    
    ! piecewise cubic spline interpolation
    call grid_Cons_Equi(p_plot, 0.1d0, 3d0)
    
    call spline_interp(q_stotal, coeff)
    do i = 0, nplot 
        q_plot(i) = spline_eval(p_plot(i), coeff, 0.1d0 , 3d0) 
    enddo 
    call plot(q_plot, p_plot, legend = 'Int Supply', color = '#47FF00')
    
    call spline_interp(d, coeff)
    do i = 0, nplot 
        d_plot(i) = spline_eval(p_plot(i), coeff, 0.1d0 , 3d0) 
    enddo 
    call plot(d_plot, p_plot, legend = 'Int demand')
    
    ! Executing the plot
    call execplot(title = 'Market Equilibrium', ylim =(/0.1d0, 3d0 /), xlim =(/0d0, 10d0 /))
contains 

end program 
