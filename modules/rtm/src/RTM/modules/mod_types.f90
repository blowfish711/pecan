module mod_types
    implicit none

    ! Parameter types
    integer, parameter :: r1 = selected_real_kind(4)
    integer, parameter :: r2 = selected_real_kind(8)
    integer, parameter :: i1 = selected_int_kind(4)
    integer, parameter :: i2 = selected_int_kind(8)

    contains

        subroutine print_matrix(x)
            real(kind=r2) :: x(:,:)
            integer(kind=i1) :: i

            do i=1,size(x, 1)
                write(*,*) x(i,:)
            enddo
            return
        end subroutine

end module

