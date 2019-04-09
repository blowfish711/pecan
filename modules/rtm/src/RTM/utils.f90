subroutine getrefractive45(out)
  USE mod_dataspec_wavelength
  USE mod_dataspec_refractive
  REAL*8 :: out(nw)
  out = refractive
end subroutine getrefractive45

subroutine getrefractived(out)
  USE mod_dataspec_wavelength
  USE mod_dataspec_prospectd
  REAL*8 :: out(nw)
  out = refractive
end subroutine getrefractived

subroutine getdataspec4(out)
  USE mod_dataspec_wavelength
  USE mod_dataspec_prospect4
  REAL*8 :: out(nw, 3)
  out(:, 1) = k_Cab
  out(:, 2) = k_Cw
  out(:, 3) = k_Cm
end subroutine

subroutine getdataspec5(out)
  USE mod_dataspec_wavelength
  USE mod_dataspec_prospect5b
  REAL*8 :: out(nw, 5)
  out(:, 1) = k_Cab
  out(:, 2) = k_Car
  out(:, 3) = k_Brown
  out(:, 4) = k_Cw
  out(:, 5) = k_Cm
end subroutine

subroutine getdataspecd(out)
  USE mod_dataspec_wavelength
  USE mod_dataspec_prospectd
  REAL*8 :: out(nw, 6)
  out(:, 1) = k_Cab
  out(:, 2) = k_Car
  out(:, 3) = k_Canth
  out(:, 4) = k_Cbrown
  out(:, 5) = k_Cw
  out(:, 6) = k_Cm
end subroutine
