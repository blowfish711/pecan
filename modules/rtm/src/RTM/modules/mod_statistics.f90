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

    ! Multivariate normal draw
    function mvrnorm(n, mu, sigma)
        integer(kind = i2), intent(in) :: n
        real(kind=r2), intent(in) :: mu(n)
        real(kind=r2), intent(in) :: sigma(n, n)
        
        real(kind=r2) :: prod(n,1)
        real(kind=r2) :: chol(n, n), mvrnorm(n)
        integer(kind=i2) :: info
        integer(kind=i2) :: i

        call cholesky(sigma, chol)

        if (info /= 0 ) then
            write(*,*) "mvnorm error"
            write(*,*) "Variance-covariance matrix is not positive definite"
            stop
        endif

        do i=1,n
            mvrnorm(i) = random_normal()
        enddo
        
        prod = blas_matmul(chol, mvrnorm, n, 1, n)
        mvrnorm = prod(:,1) + mu

        if(any(isnan(mvrnorm))) then
            write(*,*) "Bad sample in mvrnorm"
            write(*,*) "mu", mu
            write(*,*) "sigma"
            call print_matrix(sigma)
            write(*,*) "chol" 
            call print_matrix(chol)
            write(*,*) "mvrnorm", mvrnorm
            stop
        endif
        return
    end function

    subroutine cholesky (A, AB)
        real (kind=r2) :: A(:,:) 
        real (kind=r2), dimension(size(A,1), size(A,2)) :: AB

        integer (kind=i1) :: n, nn, i, j
        real (kind=r2), dimension(size(A,1)*(size(A,1)+1)/2) :: a_vec, u_vec

        integer (kind=i2) :: nullty, ifault

        n = size(A, 1)
        nn = n * (n+1) / 2
        do i=1,n
            do j=1,i
                a_vec(i*(i-1)/2 + j) = A(i,j)
            enddo
        enddo

        call cholesky_raw (a_vec, n, nn, u_vec, nullty, ifault)

        AB = 0.0d0

        ! Note this returns the UPPER triangle
        do j=1,n
            do i=1,j
                AB(i,j) = u_vec(j*(j-1)/2 + i)
            enddo
        enddo
        return
    end subroutine


    subroutine cholesky_raw (a, n, nn, u, nullty, ifault)
        integer (kind = i1) n, nn

        real (kind = r2), dimension(nn) :: a, u
        real (kind = r2), parameter :: eta = 1.0D-09
        !real (kind = r2), parameter :: zero = 1.0d-20
        integer (kind = i2) :: i, icol, ifault, ii, irow, &
            j, k, kk, l, m, nullty

        real (kind = r2) w, x

        ifault = 0
        nullty = 0
        if ( n <= 0 ) then
            ifault = 1
            return
        end if

        if ( nn < ( n * ( n + 1 ) ) / 2 ) then
            ifault = 3
            return
        end if

        j = 1
        k = 0
        ii = 0
        !
        !  Factorize column by column, ICOL = column number.
        !
        do icol = 1, n

            ii = ii + icol
            x = eta * eta * a(ii)
            l = 0
            kk = 0
            !
            !  IROW = row number within column ICOL.
            !
            do irow = 1, icol

                kk = kk + irow
                k = k + 1
                w = a(k)
                m = j

                do i = 1, irow - 1
                    l = l + 1
                    w = w - u(l) * u(m)
                    m = m + 1
                end do

                l = l + 1

                if ( irow == icol ) then
                    exit
                end if

                if ( u(l) /= 0.0D+00 ) then

                    u(k) = w / u(l)

                else

                    u(k) = 0.0D+00

                    if ( abs ( x * a(k) ) < w * w ) then
                        ifault = 2
                        return
                    end if

                end if

            end do
        !
        !  End of row, estimate relative accuracy of diagonal element.
        !
        if ( abs ( w ) <= abs ( eta * a(k) ) ) then

            u(k) = 0.0D+00
            nullty = nullty + 1

        else

            if ( w < 0.0D+00 ) then
                ifault = 2
                return
            end if

            u(k) = sqrt ( w )

        end if

        j = j + icol

        end do

        ! Assume values smaller than `tol` are zero
        !where (u < zero) u = 0.0d0

        return
        end

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
        !write(*,*) "Covariance matrix"
        !call print_matrix(cov)
        return
    end function

    ! Covariance to correlation matrix
    function cov2cor(x) result(cor)
        real(kind=r2), intent(in) :: x(:,:)
        integer(kind=i2) :: npar, i, j
        real(kind=r2), dimension(size(x,1), size(x,2)) :: diag
        real(kind=r2), dimension(size(x,1), size(x,2)) :: cor

        npar = size(x,1)
        do i=1,npar
            diag(i,i) = 1.0d0/sqrt(x(i,i))
        enddo

        cor = blas_matmul(diag, x, npar, npar, npar)
        cor = blas_matmul(cor, diag, npar, npar, npar)

        return
    end function

    function blas_matmul(A, B, M, N, K) result(C)
        ! M = nrow(A)
        ! N = ncol(B)
        ! K = ncol(A) == nrow(B)
        integer(kind=i2) :: M, N, K
        real(kind=r2), intent(in) :: A(M,K), B(K,N)
        real(kind=r2) :: C(M,N)

        call DGEMM('N', 'N', M, N, K, 1.0d0, A, M, B, K, 0.0d0, C, M)
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
