subroutine iter(c, n, a)
  complex*16, intent(in) :: c
  integer, intent(in) :: n
  complex*16, intent(out) :: a
  complex*16 :: x
  integer :: i
  x = 0
  a = 0
  do i = 1,n
     x = x * x + c
     if (x /= 0) then
        a = a + (1 / x)
     end if
     if (real(x * conjg(x)) > 4) then
        if (a /= 0) then
           a = log(i / a)
        end if
        return
     end if
  end do
  if (a /= 0) then
     a = log(n / a)
  end if
end subroutine iter


program mandel
  implicit none
  integer :: iters = 1024
  integer :: scale = 256
  integer :: yi, xi, gs, n
  complex*16 :: c, a
  double precision :: y, x, th, tau
  tau = 2 * acos(0.0)
  write (*,"(A)") "P2"
  write (*,*) (5 * scale / 2) + 1, " ", (5 * scale / 2) + 1
  write (*,*) 255
  do yi = -5 * scale / 4, 5 * scale / 4, 1
     do xi = -2 * scale, scale / 2, 1
        y = yi / dble(scale)
        x = xi / dble(scale)
        n = iters
        c = cmplx(x, y)
        call iter(c, n, a)
!        write (*,*) "# c = ", c, ", n = ", n
        gs = modulo(real(a) * 1.5 + imag(a) / tau, 1.0) * 256
        write (*,*) gs
     end do
  end do
end program mandel
