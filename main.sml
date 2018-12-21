val StatusRegister  = {N=false, V=false, B5=true, B4=false, D=false, I=true, Z=false, C=false}
type cpuData = {regs : CPU.registers, cycle : int}
val CYCLES_PER_FRAME = 263 * 341 div 3 - 1 

fun printScreen fb = 
    let
      val a = Array.length fb
      fun loop 0 = print "---The end of a frame---\n"
        | loop n =
         (
          (* print (Word.toString (Array.sub (fb, a-n)) ^ "\n"); *)
          if 0wx80 <= Array.sub (fb, a-n) then print "." else print " ";
          if (a-n) mod 256 = 255 then (print "|\n"; loop (n-1)) else loop (n-1))
    in
      loop a
    end
    
fun frame cData =
    let
      val (newCpuRegs, newCpuCycle) = CPU.cpu (#regs cData, #cycle cData)
    in
        if newCpuCycle > CYCLES_PER_FRAME
            then (printScreen (PPU.ppu ());
                  (* print (Word8.toString (PPU.read 0wx3F00) ^ "\n");
                  print (Word8.toString (PPU.read 0wx21C9) ^ "\n");
                  print (Word8.toString (PPU.read 0wx21CA) ^ "\n");
                  print (Word8.toString (PPU.read 0wx21CB) ^ "\n");
                  print (Word8.toString (PPU.read 0wx21CC) ^ "\n");
                  print (Word8.toString (PPU.read 0wx21CD) ^ "\n");
                  print (Word8.toString (PPU.read 0wx21CE) ^ "\n");
                  print (Word8.toString (PPU.read 0wx21DF) ^ "\n");
                  print (Word8.toString (PPU.read 0wx21D1) ^ "\n");
                  print (Word8.toString (PPU.read 0wx21D2) ^ "\n");
                  print (Word8.toString (PPU.read 0wx21D3) ^ "\n");
                  print (Word8.toString (PPU.read 0wx21D4) ^ "\n");
                  print (Word8.toString (PPU.read 0wx21D5) ^ "\n"); *)
                  frame (cData # {regs = newCpuRegs, cycle = 0}))
        else frame (cData # {regs = newCpuRegs, cycle = newCpuCycle})
    end

fun run _ =
    (
      Mapper.init ();
      frame {regs = {A=0w0, X=0w0, Y=0w0, PC= CPU.read16 0wxfffc, S=0wxFD, P=StatusRegister}, cycle = 27280}
    )

val _ = run ()