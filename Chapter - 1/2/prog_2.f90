program prog_2 

    implicit none 
    real:: tempa = 55555553.00, tempb = 10000001.00 , sum1
        
    real*8 :: tempc = 55555553d0, tempd = 10000001d0, sum2
    
    sum1 = tempa + tempb
    sum2 = tempc + tempd 
    
    write(*,*) sum1
    write(*,*) sum2 
    

end program 
