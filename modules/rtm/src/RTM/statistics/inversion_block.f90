subroutine invert_block(observed, nspec, modcode, &
            inits, npars, ipars, cons, ncons, icons, &
            pmu, psd, plog, pmin, ngibbs, results, seed)
    use mod_types
    use mod_statistics
    use mod_selectmodel
    use mod_dataspec_wavelength
    implicit none

    ! Inputs
    integer(kind=i2), intent(in) :: nspec, npars, ncons, modcode, seed(100)
    integer(kind=i2), intent(in) :: ipars(npars), icons(ncons)
    real(kind=r2), intent(in) :: observed(nw,nspec), inits(npars), cons(ncons)
    real(kind=r2), intent(in), dimension(npars) :: pmin, pmu, psd
    logical, intent(in) :: plog(npars)
    integer(kind=i2), intent(in) :: ngibbs

    ! Internals
    integer(kind=i2) :: i, ng, adapt
    integer(kind=i2) :: nseed
    real(kind=r2) :: rp1, rp2, rinv, rsd, rp_prior_1, rp_prior_2
    real(kind=r2) :: PrevError(nw,nspec), PrevSpec(nw)
    real(kind=r2), dimension(npars, npars) :: Jump, rescale, cormat
    real(kind=r2) :: adj_min
    real(kind=r2) :: adj, ar
    procedure(), pointer :: model

    ! Outputs
    real(kind=r2), intent(out) :: results(ngibbs, npars+1)

    nseed = 100
    call random_seed(size=nseed)
    call random_seed(put=seed)      !! Initialize random number generator
    call model_select(modcode, model) 

    rp_prior_1 = 0.001
    rp_prior_2 = 0.001
    rp1 = rp_prior_1 + nspec*nw/2
    rsd = 0.5
    call model(inits, npars, ipars, cons, ncons, icons, PrevSpec)
    do i=1,nspec
        PrevError(:,i) = PrevSpec - observed(:,i)
    enddo
    !! TODO: Make Jump an input and output (for resuming runs)
    Jump = 0d0
    do i=1,npars
        Jump(i,i) = inits(i) * 0.05
    enddo
    rescale = 0d0
    adapt = 50
    adj_min = 0.1
    ar = 0
    do ng=1,ngibbs
        if (ng > adapt .AND. mod(ng, adapt) == 1) then
            rescale = 0d0
            write(*,*) "Acceptance rate", ar
            if(ar < 2) then
                do i=1,npars
                    rescale(i,i) = adj_min
                enddo
                Jump = matmul(rescale, Jump)
                Jump = matmul(Jump, rescale)
            else
                adj = ar / adapt / 0.234
                if (adj < adj_min) adj = adj_min
                write(*,*) "Adjustment", adj
                do i=1,npars
                    rescale(i,i) = sd(results(ng-adapt+1 : ng-1,i)) * adj
                enddo
                write(*,*) "rescale"
                call print_matrix(rescale)
                cormat = cor(results(ng-adapt : ng-1, 1:npars))
                write(*,*) "cormat"
                call print_matrix(cormat)
                if (any(isnan(cormat))) then
                    write(*,*) "Some cormat values are NAN. Converting to identity"
                    cormat = 0d0
                    do i = 1,npars
                        cormat(i,i) = 1.0d0
                    enddo
                endif
                Jump = matmul(rescale, cormat)
                Jump = matmul(Jump, rescale)
            endif
            write(*,*) "Jump"
            call print_matrix(Jump)
            ar = 0d0
        endif
        call mh_sample_block(observed, nspec, model, &
            inits, npars, ipars, cons, ncons, icons, rsd, &
            Jump, pmu, psd, plog, pmin, PrevError, ar)
        results(ng,1:npars) = inits
        rp2 = rp_prior_2 + sum(PrevError * PrevError)/2.0d0
        rinv = rgamma(rp1, 1/rp2)
        rsd = 1.0d0/sqrt(rinv)
        results(ng,npars+1) = rsd
    enddo
end subroutine
