structure Mapper =
struct
  fun init _ =
      let
        val file = BinIO.openIn "./sample1/sample1.nes"
        val cartridge  : word8 vector = BinIO.inputAll file
        val _ = BinIO.closeIn file

        val promUnits = Vector.sub (cartridge, 4)
        val cromStart = 16 + 16384 * Word8.toInt promUnits 
        fun processCrom 8192 = ()
          | processCrom n =
            (
              Array.update (PPU.chrRom, n, Vector.sub (cartridge, n + cromStart));
              processCrom (n+1)
            )
        fun mapper0 _ =
            let
              fun nrom128 n =
                  let
                    fun processProm 16384 = ()
                      | processProm n =
                        let
                          val subed =  Vector.sub (cartridge, n+16)
                          val target = n + Word.toInt (0wx8000-0wx4020)
                        in
                          (
                            Array.update (CPU.prgRom, target, subed);
                            Array.update (CPU.prgRom, target + 16384, subed);
                            processProm (n+1)
                          ) 
                        end
                  in
                    (processProm n; processCrom n)
                  end
              fun nrom256 n =
                  let
                    fun processProm 32768 = ()
                      | processProm n =
                        let
                          val subed =  Vector.sub (cartridge, n+16)
                          val target = n + Word.toInt (0wx8000-0wx4020)
                        in
                        (
                          Array.update (CPU.prgRom, target, subed);
                          processProm (n+1)
                        ) 
                        end
                  in
                    (processProm n; processCrom n)
                  end
            in
              case promUnits of
                0w1 => nrom128 0
              | 0w2 => nrom256 0
              | _   => ()
            end
      in
        mapper0 ()
      end
end