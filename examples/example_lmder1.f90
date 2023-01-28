module testmod_der1
use minpack, only: chkder
implicit none

integer, parameter :: dp=kind(0d0)

contains

subroutine fcn2(m, n, x, fvec, fjac, ldfjac, iflag)
integer, intent(in) :: m, n, ldfjac, iflag
real(dp), intent(in) :: x(n)
real(dp), intent(out) :: fvec(m), fjac(ldfjac, n)

integer :: i
real(dp) :: tmp1, tmp2, tmp3, tmp4, y(15)
y = [1.4D-1, 1.8D-1, 2.2D-1, 2.5D-1, 2.9D-1, 3.2D-1, 3.5D-1, 3.9D-1, &
        3.7D-1, 5.8D-1, 7.3D-1, 9.6D-1, 1.34D0, 2.1D0, 4.39D0]

if (iflag == 1) then
    do i = 1, 15
        tmp1 = i
        tmp2 = 16 - i
        tmp3 = tmp1
        if (i .gt. 8) tmp3 = tmp2
        fvec(i) = y(i) - (x(1) + tmp1/(x(2)*tmp2 + x(3)*tmp3))
    end do
else
    do i = 1, 15
        tmp1 = i
        tmp2 = 16 - i
        tmp3 = tmp1
        if (i .gt. 8) tmp3 = tmp2
        tmp4 = (x(2)*tmp2 + x(3)*tmp3)**2
        fjac(i,1) = -1.D0
        fjac(i,2) = tmp1*tmp2/tmp4
        fjac(i,3) = tmp1*tmp3/tmp4
    end do
end if
end subroutine

subroutine check_deriv(x, fvec, fjac)
real(dp), intent(in) :: x(:)
real(dp), intent(out) :: fvec(:), fjac(:,:)
real(dp) :: xp(size(x)), fvecp(size(fvec)), err(size(fvec))
integer :: i
call chkder(size(fvec), size(x), x, fvec, fjac, size(fjac, 1), xp, fvecp, &
    1, err)
call fcn2(size(fvec), size(x), x, fvec, fjac, size(fjac, 1), 1)
call fcn2(size(fvec), size(x), x, fvec, fjac, size(fjac, 1), 2)
call fcn2(size(fvec), size(x), xp, fvecp, fjac, size(fjac, 1), 1)
call chkder(size(fvec), size(x), x, fvec, fjac, size(fjac, 1), xp, fvecp, &
    2, err)
print *, "Derivatives check (1.0 is correct, 0.0 is incorrect):"
print *, err
do i = 1, size(err)
    if (abs(err(i) - 1) > 1e-15_dp) error stop
end do
end subroutine

end module


program example_lmder1
use minpack, only: enorm, lmder1, chkder
use testmod_der1, only: dp, fcn2, check_deriv
implicit none

integer :: info
real(dp) :: tol, x(3), fvec(15), fjac(size(fvec), size(x)), eps, err
integer :: ipvt(size(x))
real(dp), allocatable :: wa(:)

! The following starting values provide a rough fit.
x = [1._dp, 1._dp, 1._dp]

call check_deriv(x, fvec, fjac)

! Set tol to the square root of the machine precision. Unless high precision
! solutions are required, this is the recommended setting.
tol = sqrt(epsilon(1._dp))

allocate(wa(5*size(x) + size(fvec)))
call lmder1(fcn2, size(fvec), size(x), x, fvec, fjac, size(fjac, 1), tol, &
    info, ipvt, wa, size(wa))
print 1000, enorm(size(fvec), fvec), info, x
1000 format(5x, 'FINAL L2 NORM OF THE RESIDUALS', d15.7 // &
            5x, 'EXIT PARAMETER', 16x, i10              // &
            5x, 'FINAL APPROXIMATE SOLUTION'            // &
            5x, 3d15.7)

eps = 2.2e-16_dp ! epsilon(1._dp)
print *, "sum(x) = ", sum2(x)
err = abs(sum2(x) - (3.5591418689884917_dp))
print *, "sum(x) error: ", err
if (err > eps) error stop

contains

    real(dp) function sum2(x) result(r)
    real(dp), intent(in) :: x(:)
    integer :: i
    r = 0
    do i = 1, size(x)
        r = r + x(i)
    end do
    end function

end program
