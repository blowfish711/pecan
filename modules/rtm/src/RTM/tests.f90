subroutine test_mvrnorm(n, mu, sigma, sample)
    use mod_statistics
    integer(kind=i2), intent(in) :: n
    real(kind=r2), intent(in) :: mu(n), sigma(n,n)
    real(kind=r2), intent(out) :: sample(n)
    sample = mvrnorm(n, mu, sigma)
end subroutine


