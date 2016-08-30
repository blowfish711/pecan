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

    function mvrnorm(n, mu, sigma)
        integer(kind=i2), intent(in) :: n
        real(kind=r2), intent(in) :: mu(n), sigma(n,n)

        real(kind=r1) :: mu2(n)
        real(kind=r1), dimension(n * (n-1) / 2 + n) :: sigma_vec, lower_vec
        integer(kind=i2) :: i, j, posdef

        real(kind=r1) :: mvrnorm(n)

        ! random_mvnorm takes real(4) for arguments, so coerce
        mu2 = mu

        ! Convert sigma to vector of upper triangular matrix
        do i = 1, n
            do j = i, n
                sigma_vec(j * (j-1) / 2 + i) = sigma(i,j)
            enddo
        enddo

        call random_mvnorm(n, mu2, sigma_vec, lower_vec, .true., mvrnorm, posdef)
        return
    end function

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

end module        
