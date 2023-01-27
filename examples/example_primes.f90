module types
implicit none
private
public dp

integer, parameter :: dp=kind(0d0)

end module

module find_fit_module

! This module contains a general function find_fit() for a nonlinear least
! squares fitting. The function can fit any nonlinear expression to any data.

use minpack, only: lmdif1
use types, only: dp
implicit none
private
public find_fit

contains

subroutine find_fit(data_x, data_y, pars)
real(dp), intent(in) :: data_x(:), data_y(:)
real(dp), intent(inout) :: pars(:)

real(dp) :: tol, fvec(size(data_x))
integer :: iwa(size(pars)), info, m, n
real(dp), allocatable :: wa(:)

tol = sqrt(epsilon(1._dp))
m = size(fvec)
n = size(pars)
allocate(wa(m*n + 5*n + m))
call lmdif1(fcn, m, n, pars, fvec, tol, info, iwa, wa, size(wa))
if (info /= 1) stop "failed to converge"

contains

subroutine fcn(m, n, x, fvec, iflag)
integer, intent(in) :: m, n, iflag
real(dp), intent(in) :: x(n)
real(dp), intent(out) :: fvec(m)
real(dp) :: y(m), data_y2(m)
real(dp) :: a, b, c
integer :: i
! Suppress compiler warning:
fvec(1) = iflag
data_y2 = data_y

a = x(1)
b = x(2)
c = x(3)
do i = 1, m
!    y(i) = a*data_x(i)*log(b + c*data_x(i))
    fvec(i) = data_y2(i) - y(i)
end do
end subroutine

end subroutine

end module


program example_primes

! Find a nonlinear fit of the form a*x*log(b + c*x) to a list of primes.

use find_fit_module, only: find_fit
use types, only: dp
implicit none

real(dp) :: pars(3)
integer, parameter :: y2(*) = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, &
    37, 41, 43, 47, 53, 59, 61, 67, 71]
real(dp) :: x(20), y(20), err, eps
integer :: i
y = real(y2, dp)
pars = [1._dp, 1._dp, 1._dp]
do i = 1, size(y)
    x(i) = i
end do
call find_fit(x, y, pars)
print *, pars

eps = 2.2e-16_dp ! epsilon(1._dp)
err = abs(pars(1) - (1.4207732518240344_dp))
print *, "pars(1) error: ", err
if (err > eps) error stop
err = abs(pars(2) - (1.6556110935635728_dp))
print *, "pars(2) error: ", err
if (err > eps) error stop
err = abs(pars(3) - (0.53462503550719431_dp))
print *, "pars(3) error: ", err
if (err > eps) error stop

end program
