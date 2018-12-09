structure PPU =
struct
  val cycle 
  fun ppu (oldCycle, cycle, line) =
    if (cycle + oldCycle >= 341) then 
        (cycle -= 341; line += 1)
    else 
    
end