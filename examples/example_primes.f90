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

subroutine find_fit(data_x, data_y, expr, pars)
! Fits the (data_x, data_y) arrays with the function expr(x, pars).
! The user can provide any nonlinear function 'expr' depending on any number of
! parameters 'pars' and it must return the evaluated expression on the
! array 'x'. The arrays 'data_x' and 'data_y' must have the same
! length.
real(dp), intent(in) :: data_x(:), data_y(:)
interface
    subroutine expr(x, pars, y)
    use types, only: dp
    implicit none
    real(dp), intent(in) :: x(:), pars(:)
    real(dp), intent(out) :: y(:)
    end subroutine
end interface
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
real(dp) :: y(m)
real(dp) :: data_x2(m), data_y2(m)
! Suppress compiler warning:
fvec(1) = iflag
!data_x2 = data_x
!data_y2 = data_y
!call expr(data_x2, x, y)
!fvec = data_y2 - y
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
real(dp) :: y(20)
integer :: i
y = real(y2, dp)
pars = [1._dp, 1._dp, 1._dp]
!call find_fit([(real(i, dp), i=1,size(y))], y, expression, pars)
print *, pars

contains

!subroutine expression(x, pars, y)
!real(dp), intent(in) :: x(:), pars(:)
!real(dp), intent(out) :: y(:)
!real(dp) :: a, b, c
!a = pars(1)
!b = pars(2)
!c = pars(3)
!y = a*x*log(b + c*x)
!end subroutine

end program
