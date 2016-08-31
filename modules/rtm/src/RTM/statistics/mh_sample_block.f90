subroutine mh_sample_block(observed, nspec, model, &
        inits, npars, ipars, cons, ncons, icons, rsd, &
        Jump, pmu, psd, plog, pmin, PrevError, ar)
    use mod_types
    use mod_statistics
    use mod_dataspec_wavelength
    implicit none

    ! Inputs -- unchanged
    integer(kind=i2), intent(in) :: npars, nspec, ncons
    integer(kind=i2), intent(in) :: ipars(npars), icons(ncons)
    real(kind=r2), intent(in) :: observed(nw,nspec), cons(ncons), rsd, Jump(npars, npars)
    real(kind=r2), intent(in), dimension(npars) :: pmin, pmu, psd
    logical, intent(in) :: plog(npars)
    procedure() :: model

    ! Input/Output -- modified
    real(kind=r2) :: ar
    real(kind=r2) :: inits(npars), PrevError(nw, nspec)

    ! Internals
    integer(kind=i1) :: p, i, j
    real(kind=r2) :: tvec(npars), a, u
    real(kind=r2) :: TryError(nw,nspec), TrySpec(nw)
    real(kind=r2) :: TryPost, PrevPost

    if(any(inits < pmin)) then
        write(*,*) "Illegal initial values", inits
        stop
    endif
    tvec = mvrnorm(npars, inits, Jump)
    if (ar == 0) then
        write(*,*) "tvec", tvec
        write(*,*) "inits", inits
        write(*,*) "difference", tvec - inits
    endif
    if(any(tvec < pmin)) return
    call model(tvec, npars, ipars, cons, ncons, icons, TrySpec)
    do i = 1,nspec
        TryError(:,i) = TrySpec - observed(:,i)
    enddo
    call prior_block(npars, tvec, pmu, psd, plog, TryPost)
    call prior_block(npars, inits, pmu, psd, plog, PrevPost)
    do i=1,nw
        do j=1,nspec
            TryPost = TryPost + ldnorm(TryError(i,j), 0d0, rsd)
            PrevPost = PrevPost + ldnorm(PrevError(i,j), 0d0, rsd)
        enddo
    enddo
    a = exp(TryPost - PrevPost)
    if (isnan(a)) return
    call random_number(u)
    if(a > u) then
        inits = tvec
        PrevError = TryError
        ar = ar + 1.0d0
    endif
    return
end subroutine

