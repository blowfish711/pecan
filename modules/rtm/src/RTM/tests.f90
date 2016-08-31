subroutine test_mvrnorm(nsamp, nvar, mu, sigma, sample)
    use mod_statistics
    use mod_types
    integer(kind=i2), intent(in) :: nsamp, nvar
    real(kind=r2), intent(in) :: mu(nvar), sigma(nvar, nvar)
    real(kind=r2), intent(out) :: sample(nsamp, nvar)
    integer(kind=i2) :: i
    do i=1,nsamp
        sample(i,:) = mvrnorm(nvar, mu, sigma)
    enddo
    return
end subroutine


