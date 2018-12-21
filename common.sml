structure Common =
struct
  fun rd (device, addr): word8 = Array.sub (device, Word16.toInt addr)
  fun wr (device, addr, value): unit = Array.update (device, Word16.toInt addr, value)
  exception MemFault
  fun pow (a, 0) = 1
    | pow (a, b) = a * pow (a, b-1)
end