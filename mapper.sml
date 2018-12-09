structure Mapper =
struct
  fun init _ =
      let
        val file = BinIO.openIn "../../sample1/sample1.nes"
        val prgrom  : word8 vector = BinIO.inputAll file
        val _ = BinIO.closeIn file
        fun mapper0 n =
            let
              val units = Vector.sub (prgrom, 4)
              fun nrom128 16384 = ()
                  | nrom128 n =
                  let
                  val subed =  Vector.sub (prgrom, n+16)
                  val target = n + Word.toInt (0wx8000-0wx4020)
                  in
                  (Array.update (CPU.cartridge, target, subed);
                  Array.update (CPU.cartridge, target + 16384, subed);
                  nrom128 (n+1)) 
                  end
              fun nrom256 32768 = ()
                | nrom256 n =
                  let
                      val subed =  Vector.sub (prgrom, n+16)
                      val target = n + Word.toInt (0wx8000-0wx4020)
                  in
                      (Array.update (CPU.cartridge, target, subed);
                      nrom256 (n+1)) 
                  end
            in
              case units of
                0w1 => nrom128 n
              | 0w2 => nrom256 n
              | _   => ()
            end
      in
        mapper0 0
      end
end