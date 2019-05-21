program prog_8
    implicit none 
    
    real*8 :: x
    integer :: n, iter = 1000000, p, q ,d , xx , yy, res , ncount1, ncount2   
    real*8 :: Dsum(2:12), ocurrs(0:1)   
    
    ! conditions 
    xx = 3 
    yy = 12

    n = 1 
    do
        call random_number(x)
        p = int(6*x) + 1 
        call random_number(x)
        q = int(6*x) + 1 
        
        d = p + q
        Dsum(d) = Dsum(d) + 1   
        
        if (d == xx) then 
            ocurrs(0) = ocurrs(0) + 1
        elseif (d == yy) then 
            ocurrs(1) = ocurrs(1) + 1
        endif 
        n = n + 1 
        if (n == iter) exit 
    enddo
    write(*,*) 'The conditions are: '
    write(*,'(a, i3 / a, i3)') ' X: ', xx, ' Y: ', yy  
    
    write(*,*) '--------------------------------------------'
    write(*,*) '--------------------------------------------'
    write(*,'(3a)') '  n ','     Dsum ', '     prob(Dsum)'
    do n = 2, 12
        write(*,'(i3, a, i10, a, f12.4)') n ,' ', int(Dsum(n)), ' ', dble(Dsum(n))/dble(iter) 
    enddo

    write(*,*) '--------------------------------------------'
    write(*,*) 'Who often the game will be stopped by the first and second condition? '
    do n = 1, 2
        write(*,'(a, i3 , a, f12.4)') ' Condition ', n, ': ',dble(ocurrs(n-1))/dble(iter) 
    enddo
    write(*,*) '--------------------------------------------'
    write(*,*) '--------------------------------------------'
    write(*,*) 'Random_int function test: '
    ncount1 = 0 
    ncount2 = 0 
    do 
        call random_int(res, xx, yy)  
        ncount2 = ncount2 + 1 
        if (res /= 0) then 
            ncount1 = ncount1 + 1 
        elseif (res == 0 ) then 
            exit 
        endif 
    enddo 
    write(*,*) '--------------------------------------------'
    write(*,'(a, i3)') ' Number of rolls before the game stop: ', ncount1

contains 
    subroutine random_int(res, int0, inth) 
        implicit none 
        
        ! input arguments 
        integer, intent(in) :: int0, inth  
        integer, intent(inout) :: res
        
        ! local variables 
        integer :: p , q, d 
        real*8 :: x
        
        ! executable code
        do 
            call random_number(x)
            p = int(6*x) + 1 
            call random_number(x)
            q = int(6*x) + 1 
        
            d = p + q
            if ((int0 > inth .or. inth > int0 ) .and. (d /= int0 .and. d /= inth)) then 
                res = d
                exit 
            elseif ((int0 == inth) .or. ((int0 == 2) .and. (inth == 2)))   then 
                write(*,*) 'You need to put different limits! '
                exit
            else 
                res = 0
                exit 
            endif 
        enddo
    end subroutine 
end program 
