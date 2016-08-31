module mod_statistics
    use mod_types
    use random
    implicit none

    contains 

    ! Random number generation
    function rnorm(mu, sigma)
        real(kind=r2), intent(in) :: mu, sigma
        real(kind=r2) :: rnorm

        rnorm = mu + random_normal() * sigma
        return
    end function

!    function mvrnorm(n, mu, sigma)
!        integer(kind=i2), intent(in) :: n
!        real(kind=r2), intent(in) :: mu(n), sigma(n,n)
!
!        !real(kind=r2), dimension(n * (n-1) / 2 + n) :: sigma_vec, lower_vec
!        !integer(kind=i2) :: i, j, posdef
!
!        real(kind=r2) :: mvrnorm(n)
!
!        ! Convert sigma to vector of upper triangular matrix
!        !do i = 1, n
!        !    do j = i, n
!        !        sigma_vec(j * (j-1) / 2 + i) = sigma(i,j)
!        !    enddo
!        !enddo
!
!        call random_mvnorm(n, mu, sigma, lower_vec, .true., mvrnorm, posdef)
!        if(any(isnan(mvrnorm))) then
!            write(*,*) "Bad sample in mvrnorm"
!            write(*,*) "mu", mu
!            write(*,*) "sigma", sigma
!            write(*,*) "sigma_vec", sigma_vec
!            write(*,*) "lower_vec", lower_vec
!            write(*,*) "mvrnorm", mvrnorm
!            write(*,*) "posdef", posdef
!            stop
!        endif
!        return
!    end function

    function rgamma(shp, scl)
        ! NOTE: random_gamma takes real(4) as argument for shape.
        ! Therefore, coerce real(8) (r2) argument `shp` to real(4) (r1)
        ! by copying.
        real(kind=r2), intent(in) :: shp, scl
        real(kind=r1) :: shp2 
        real(kind=r2) :: rgamma

        shp2 = shp
        rgamma = random_gamma(shp2, .true.) * scl
        return
    end function

    ! Density calculation
    function ldnorm(x, mu, sigma)
        real(kind=r2), intent(in) :: x, mu, sigma
        real(kind=r2) :: hlog2pi, lsig, xm, s2, ex
        real(kind=r2) :: ldnorm

        hlog2pi = -0.39908993417 !! -0.5 * log(2pi)
        lsig = -log(sigma)
        xm = x - mu
        s2 = 2 * sigma * sigma
        ex = -xm * xm / s2
        ldnorm = hlog2pi + lsig + ex
        return
    end function

    ! Mean
    function mean(x)
        real(kind=r2), intent(in) :: x(:)
        real(kind=r2) :: mean
        mean = sum(x) / size(x)
        return
    end function

    ! Standard deviation
    function sd(x)
        real(kind=r2), intent(in) :: x(:)
        real(kind=r2), dimension(size(x)) :: dif, dif2
        real(kind=r2) :: mu, sd, var

        mu = mean(x)
        dif = x - mu
        dif2 = dif * dif
        var = sum(dif2) / (size(x) - 1.0d0)
        sd = sqrt(var)
        return
    end function


    ! Variance-Covariance matrix
    function cov(x)
        real(kind=r2), intent(in) :: x(:,:)
        integer(kind=i1) :: nrow, ncol, i, j
        real(kind=r2) :: mu
        real(kind=r2), dimension(size(x,1), size(x,2)) :: dif_matrix

        real(kind=r2), dimension(size(x,2), size(x,2)) :: cov

        nrow = size(x, 1)
        ncol = size(x, 2)

        do j=1,ncol
            mu = mean(x(:,j))
            dif_matrix(:,j) = x(:,j) - mu
        enddo

        do i=1,ncol
            do j=i,ncol       
                cov(i,j) = sum(dif_matrix(:,i) * dif_matrix(:,j)) / (nrow - 1.0d0)
                cov(j,i) = cov(i,j)
            enddo
        enddo
        return
    end function

    ! Covariance to correlation matrix
    function cov2cor(x) result(cor)
        real(kind=r2), intent(in) :: x(:,:)
        integer(kind=i1) :: npar, i, j
        real(kind=r2), dimension(size(x,1), size(x,2)) :: diag
        real(kind=r2), dimension(size(x,1), size(x,2)) :: cor

        npar = size(x,1)
        do i=1,npar
            diag(i,i) = 1.0d0/sqrt(x(i,i))
        enddo

        cor = matmul(diag, x)
        cor = matmul(cor, diag)

        ! Hack to get around numerical instability
        ! Force positive-definite by matching top and bottom halves
!        do i=1,npar
!            do j=i,npar
!                if(i == j) cor(j,i) = 1.0d0
!                cor(j,i) = cor(i,j)
!            enddo
!        enddo

        return
    end function

    ! Correlation matrix
    function cor(x)
        real(kind=r2), intent(in) :: x(:,:)
        real(kind=r2), dimension(size(x,2), size(x,2)) :: covmat, cor

        integer(kind=i1) :: n

        n = size(x,1)

        covmat = cov(x)
        cor = cov2cor(covmat)
        return

    end function

end module        
