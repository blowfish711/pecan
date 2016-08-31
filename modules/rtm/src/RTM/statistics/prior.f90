! Custom, (log)normal priors
subroutine prior(x, pmu, psd, lognorm, d)
    use mod_types
    use mod_statistics
    implicit none

    real(kind=r2), intent(in) :: x, pmu, psd
    logical, intent(in) :: lognorm
    real(kind=r2), intent(out) :: d

    real(kind=r2) :: xx

    if(lognorm) then
        xx = log(x)
    else 
        xx = x
    end if

    d = ldnorm(xx, pmu, psd)
    return
end subroutine

subroutine prior_block(n, x, pmu, psd, lognorm, d)
    use mod_types
    use mod_statistics
    implicit none

    integer(kind=i2), intent(in) :: n
    real(kind=r2), intent(in), dimension(n) :: x, pmu, psd
    logical, intent(in), dimension(n) :: lognorm
    
    real(kind=r2), intent(out) :: d

    integer(kind=i1) :: p
    real(kind=r2) :: dadd

    d = 0d0
    do p=1,n
        call prior(x(p), pmu(p), psd(p), lognorm(p), dadd)
        d = d + dadd
    enddo
    return
end subroutine
        

subroutine prior_re(x, tausd, d)
    use mod_types
    use mod_statistics
    implicit none

    real(kind=r2), intent(in) :: x, tausd
    real(kind=r2), intent(out) :: d

    d = ldnorm(x, 0.0d0, tausd)
    return
end subroutine
