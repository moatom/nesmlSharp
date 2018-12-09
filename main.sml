val StatusRegister  = {N=false, V=false, B5=true, B4=false, D=false, I=true, Z=false, C=false}
 
fun frame (regs, cycle) =
    let
      val (newRegs, newCycle) = CPU.cpu regs
      (* val updatedCycle = PPU.ppu ((cycle+newCycle)*3) *)
    in
      ()
      (* frame (newRegs, updatedCycle) *)
    end


fun run _ =
    (
      Mapper.init ();
      frame ({A=0w0, X=0w0, Y=0w0, PC= CPU.read16 0wxfffc, S=0wxFD, P=StatusRegister}, 0)
    )

val _ = run ()