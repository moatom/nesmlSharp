val StatusRegister  = {N=false, V=false, B5=true, B4=false, D=false, I=true, Z=false, C=false}
type cpuData = {regs : CPU.registers, cycle : int}
val CYCLES_PER_FRAME = 263 * 341 div 3

val srect = {1_x = 0, 2_y = 0, 3_w = 256, 4_h = 240}
    (* fun grad i =
        let val x = real (i mod (#3_w srect)) / real (#3_w srect)
            val y = real (i div (#3_w srect)) / real (#4_h srect)
            val r = Word32.fromInt (trunc (x * 256.0))
            val b = Word32.fromInt (trunc (y * 256.0))
            val rgb = Word32.orb (Word32.<< (r, 0w16), b)
        in Word32.orb (Word32.<< (rgb, 0w8), 0w255)
        end *)
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
(* val a = Array.tabulate (#3_w srect * #4_h srect, grad) *)

fun printScreen fb =
    let
      val _ = SDL.updateTexture (t, srect, fb, 4 * #3_w srect)
      val FRAME_INTERVAL = 0w16 (* 60.25 fps *)
      fun eventLoop t1 =
          case SDL.pollEvent () of
            SOME e => if SDL.eventType e = SDL.SDL_QUIT then () else eventLoop t1
          | NONE => 
            let
              val _ = SDL.renderCopy (r, t, srect, rrect)
              val _ = SDL.renderPresent r
              val t2 = SDL.getTicks ()
            in
              if t2 - t1 >= FRAME_INTERVAL
              then eventLoop (t1 + Word32.mod (t2 - t1, FRAME_INTERVAL))
              else (SDL.delay (t1 + FRAME_INTERVAL - t2);
                    eventLoop (t1 + FRAME_INTERVAL))
            end

      val _ = eventLoop (SDL.getTicks ())
      val _ = SDL.destroyTexture t
      val _ = SDL.destroyRenderer r
      val _ = SDL.destroyWindow w
      val _ = SDL.quit ()
    in
      ()
    end

(* fun printScreen fb = 
    let
      val a = Array.length fb
      fun loop 0 = print "---The end of a frame---\n"
        | loop n =
         (
          (* print (Word.toString (Array.sub (fb, a-n)) ^ "\n"); *)
          if 0wx80 <= Array.sub (fb, a-n) then print "¶" else print " ";
          if (a-n) mod 256 = 255 then (print "|\n"; loop (n-1)) else loop (n-1))
    in
      loop a
    end *)
    
fun working cData =
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
                  working (cData # {regs = newCpuRegs, cycle = 0}))
        else working (cData # {regs = newCpuRegs, cycle = newCpuCycle})
    end

fun run _ =
    (
      Mapper.init ();
      working {regs = {A=0w0, X=0w0, Y=0w0, PC= CPU.read16 0wxfffc, S=0wxFD, P=StatusRegister}, cycle = 27280}
    )

val _ = run ()
