
program Interpolation_tax
    use toolbox
    implicit none 
    
    integer, parameter :: n = 10, n_poli = 1000, datapoitns = 3
    real*8 :: ydata(1:datapoitns), xdata(1:datapoitns)
    real*8 :: ypoli(0:n_poli), xpoli(0:n_poli)
    
    ! get equidistan plot nodes and real data 
    call grid_Cons_Equi(xpoli, 35d0, 45d0)
    ydata = (/ 198.875d0, 199.500d0, 196.875d0 /)
    xdata = (/37d0, 42d0, 45d0 /)
    
    ! polynomial interpolation 
    ypoli = poly_interpol(xpoli, xdata, ydata)
    call plot(xpoli, ypoli, legend = 'Interpolation', color ='#060606')
    call plot(xdata, ydata, legend = 'Data Points', noline= .true., markersize = 2d0, &
              marker = 3, color = '#8A1C04')
    
    
    ! Max tax rate and max revenue 
    write(*,'(/a)') 'Which is the ideal tax rate?' 
    write(*,'((3x, a, f12.5))') 'tau:     ', xpoli(maxloc(ypoli))
    write(*,'((3x, a, f12.5))') 'revenue: ', maxval(ypoli) 
    
        
    call execplot(title = 'Tax Function')
contains 

end program 
