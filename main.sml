val StatusRegister  = {N=false, V=false, B5=true, B4=false, D=false, I=true, Z=false, C=false}
type cpuData = {regs : CPU.registers, cycle : int}
val CYCLES_PER_FRAME = 263 * 341 div 3 - 1 

fun printScreen pic = ()
fun frame cData =
    let
      val (newCpuRegs, newCpuCycle) = CPU.cpu (#regs cData, #cycle cData)
    in
        if newCpuCycle > CYCLES_PER_FRAME
            then (printScreen (PPU.ppu ());
                  frame (cData # {regs = newCpuRegs, cycle = 0}))
        else frame (cData # {regs = newCpuRegs, cycle = newCpuCycle})
    end

fun run _ =
    (
      Mapper.init ();
      frame {regs = {A=0w0, X=0w0, Y=0w0, PC= CPU.read16 0wxfffc, S=0wxFD, P=StatusRegister}, cycle = 27280}
    )

val _ = run ()