structure PPU =
struct
  val chrRom = Array.array (Word.toInt 0wx2000, 0wx0: word8)
  val vRam = Array.array (8192, 0w0: word8)
  val buf2006 = ref (0w0 : word16)

(* decoder *)
  fun read addr =
           if addr < 0wx2000 then Common.rd (chrRom, addr)
      else if addr < 0wx4000 then Common.rd (vRam, addr - 0wx2000)
      else raise Common.MemFault

  fun write (addr, value) =
           if addr < 0wx2000 then raise Common.MemFault
      else if addr < 0wx4000 then Common.wr (vRam, addr - 0wx2000, value)
      else raise Common.MemFault

  fun w8ToW16 w8 = Word16.fromInt (Word8.toInt w8)
  fun w16ToW8 w16 = Word8.fromInt (Word16.toInt w16)
  fun ioWrite (0wx2000, value) = ()
    | ioWrite (0wx2001, value) = ()
    | ioWrite (0wx2002, value) = ()
    | ioWrite (0wx2003, value) = ()
    | ioWrite (0wx2004, value) = ()
    | ioWrite (0wx2005, value) = ()
    | ioWrite (0wx2006, value) = buf2006 := Word16.orb (Word16.<< (!buf2006, 0w8), w8ToW16 value)
    | ioWrite (0wx2007, value) = write (!buf2006, value)

  val colors = Vector.fromList [ 
      0wx545454, 0wx1e74, 0wx81090, 0wx300088, 0wx440064, 0wx5c0030, 0wx540400, 0wx3c1800, 0wx202a00, 0wx83a00, 0wx4000, 0wx3c00, 0wx323c, 0wx0, 0wx989698,
      0wx84cc4, 0wx3032ec, 0wx5c1ee4, 0wx8814b0, 0wxa01464, 0wx982220, 0wx783c00, 0wx545a00, 0wx287200, 0wx87c00, 0wx7628, 0wx6678, 0wx0, 0wxeceeec, 0wx4c9aec,
      0wx787cec, 0wxb062ec, 0wxe454ec, 0wxec58b4, 0wxec6a64, 0wxd48820, 0wxa0aa00, 0wx74c400, 0wx4cd020, 0wx38cc6c, 0wx38b4cc, 0wx3c3c3c, 0wxeceeec, 0wxa8ccec, 0wxbcbcec, 
      0wxd4b2ec, 0wxecaeec, 0wxecaed4, 0wxecb4b0, 0wxe4c490, 0wxccd278, 0wxb4de78, 0wxa8e290, 0wx98e2b4, 0wxa0d6e4, 0wxa0a2a0]

      
  fun synthTile 960 j array n = array
    | synthTile i j array n =
      if  j = 8
        then synthTile (i+1) 0 array n
      else
        let
          val index = Word8.toInt (read (Word16.fromInt i + 0wx2000))
          val low  = read (Word16.fromInt (16 * index + j))
          val high = read (Word16.fromInt (16 * index + j + 8))
          fun dodo (a, n) =
              let
                fun getBoolDigi (a, b, c, d) =
                    if Word8.andb (a, b) = b
                      then c * Common.pow (10, d)
                    else 0
              in
                  getBoolDigi (a,  0wx1, n, 8)
                + getBoolDigi (a,  0wx2, n, 7)
                + getBoolDigi (a,  0wx4, n, 6)
                + getBoolDigi (a,  0wx8, n, 5)
                + getBoolDigi (a, 0wx10, n, 4)
                + getBoolDigi (a, 0wx20, n, 3)
                + getBoolDigi (a, 0wx40, n, 2)
                + getBoolDigi (a, 0wx80, n, 1)
              end
        in
          (Array.update (array, n, dodo (low, 1) + dodo (high, 2));
           synthTile i (j+1) array (n+1))
        end

  fun traceAt 64 j array = array
    | traceAt  i j array =
      let
        val base = (i mod 8)*2 + j*32
        (* at; 8bits splited  by 2bits *)
        val at = read (Word16.fromInt i + 0wx23C0)
      in
        ((if i < 56 then
          ( (* br *)
            Common.wr (array, Word16.fromInt (base+16) ,Word8.>> (at, 0w6));
            (* bl *)
            Common.wr (array, Word16.fromInt (base+17) ,Word8.>> (Word8.andb (at, 0wx3f), 0w4))
          )
          else ());
          (* tr *)
          Common.wr (array, Word16.fromInt base, Word8.>> (Word8.andb (at, 0wx0f), 0w2));
          (* tl *)
          Common.wr (array, Word16.fromInt (base+1), Word8.andb (at, 0wx03));

          traceAt (i+1) (if i mod 8 = 7 then j+1 else j) array
        )
      end

  fun ppu _ =
      let
        val frameBuf = Array.array (240*256, 0w0: word)
        val bitmap = synthTile 0 0 (Array.array (8 * Word.toInt 0wx3C0, 0:int)) 0
        val pnt = traceAt 0 0 (Array.array (16*15, 0w0:word8))(* Palettes Number resolution Table for blocks *)
        
        fun toGlay c =
            (Word.>> (c, 0w4) + (Word.>> (c, 0w2) mod 0wx100) +  (c mod 0wx100)) div 0w3
        fun getColor (pn,  0) = toGlay (Vector.sub (colors, Word8.toInt (read (0wx3F00))))
          | getColor (pn, cn) = toGlay (Vector.sub (colors, Word8.toInt (read (0wx3F01 + w8ToW16 ((0w4*pn - 0w1 + Word8.fromInt cn))))))
        fun render 7680 t b = frameBuf
          | render    l t b =
            let
              (* タイルのラインについて処理していく *)
              val cN = Array.sub (bitmap, l)
              val pN = Array.sub (pnt, b)
              val pos = (l div 256)*2048 + 8*(t mod 32)
              (* pixel 0-7 *)
              val c0 =  cN div Common.pow (10, 7)
              val c1 = (cN div Common.pow (10, 6)) mod 10
              val c2 = (cN div Common.pow (10, 5)) mod 10
              val c3 = (cN div Common.pow (10, 4)) mod 10
              val c4 = (cN div Common.pow (10, 3)) mod 10
              val c5 = (cN div Common.pow (10, 2)) mod 10
              val c6 = (cN div Common.pow (10, 1)) mod 10
              val c7 = (cN div Common.pow (10, 0)) mod 10
            in
              ( Array.update (frameBuf, pos + 0, getColor (pN, c0));
                Array.update (frameBuf, pos + 1, getColor (pN, c1));
                Array.update (frameBuf, pos + 2, getColor (pN, c2));
                Array.update (frameBuf, pos + 3, getColor (pN, c3));
                Array.update (frameBuf, pos + 4, getColor (pN, c4));
                Array.update (frameBuf, pos + 5, getColor (pN, c5));
                Array.update (frameBuf, pos + 6, getColor (pN, c6));
                Array.update (frameBuf, pos + 7, getColor (pN, c7));
                render (l+1) (if l mod 8 = 7 then t+1 else t) (case b of
                                                                        119 => 0
                                                                      | _   => if t mod 2 = 1 then b+1 else b)
              )
            end
      in
        render 0 0 0
      end
end


