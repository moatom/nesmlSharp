val StatusRegister  = {N=false, V=false, B5=true, B4=false, D=false, I=true, Z=false, C=false}
type cpuData = {regs : CPU.registers, cycle : int}
val CYCLES_PER_FRAME = 263 * 341 div 3

val srect = {1_x = 0, 2_y = 0, 3_w = 256, 4_h = 240}
val _ = SDL.init SDL.SDL_INIT_VIDEO
val w = SDL.createWindow ("Nesml#",
                          SDL.SDL_WINDOWPOS_UNDEFINED,
                          SDL.SDL_WINDOWPOS_UNDEFINED,
                          #3_w srect, #4_h srect,
                          SDL.windowFlags [SDL.SDL_WINDOW_RESIZABLE,
                                          SDL.SDL_WINDOW_ALLOW_HIGHDPI])
val r = SDL.createRenderer (w, ~1, SDL.rendererFlags
                                    [SDL.SDL_RENDERER_ACCELERATED,
                                      SDL.SDL_RENDERER_PRESENTVSYNC])
val (width, height) = SDL.getRendererOutputSize r
val rrect = {1_x = 0, 2_y = 0, 3_w = width, 4_h = height}
val t = SDL.createTexture (r,
                          SDL.SDL_PIXELFORMAT_RGB888,
                          SDL.SDL_TEXTUREACCESS_STATIC,
                          #3_w srect, #4_h srect)

fun printScreen fb = SDL.updateTexture (t, srect, fb, 4 * #3_w srect)
    
fun runFrame cData =
    let
      val (newCpuRegs, newCpuCycle) = CPU.cpu (#regs cData, #cycle cData)
    in
        if newCpuCycle > CYCLES_PER_FRAME
            then (printScreen (PPU.ppu ());
                  cData # {regs = newCpuRegs, cycle = 0})
        else cData # {regs = newCpuRegs, cycle = newCpuCycle}
    end

fun run _ =
    let
      val FRAME_INTERVAL = 0w16 (* 60.25 fps *)
      fun eventLoop t1 cData1 =
          case SDL.pollEvent () of
            SOME e => if SDL.eventType e = SDL.SDL_QUIT then () else eventLoop t1 cData1
          | NONE => 
            let
              val cData2 = runFrame cData1
              val t2 = SDL.getTicks ()
              val _ = SDL.renderCopy (r, t, srect, rrect)
              val _ = SDL.renderPresent r
              val passed = t2 - t1
            in
              if passed >= FRAME_INTERVAL
              then eventLoop (t1 + Word32.mod (passed, FRAME_INTERVAL)) cData2
              else (SDL.delay (FRAME_INTERVAL - passed);
                    eventLoop (t1 + FRAME_INTERVAL) cData2)
            end

      val name = hd (CommandLine.arguments ())
    in
      (
      Mapper.init name;
      eventLoop (SDL.getTicks ()) {regs = {A=0w0, X=0w0, Y=0w0, PC= CPU.read16 0wxfffc, S=0wxFD, P=StatusRegister}, cycle = 27280};
      SDL.destroyTexture t;
      SDL.destroyRenderer r;
      SDL.destroyWindow w;
      SDL.quit ()
      )
    end

val _ = run ()
