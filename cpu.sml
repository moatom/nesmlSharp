structure CPU =
struct
  val wRam : word8 array = Array.array (4 * Word.toInt 0wx800, 0wx0: word8)
  val ppuRegisters : word8 array = Array.array (Word.toInt (0wx8+0wx1FF8), 0wx0: word8)
  val prgRom : word8 array = Array.array (Word.toInt 0wxBFE0, 0wx0: word8)

  fun w8ToW16 w8 = Word16.fromInt (Word8.toInt w8)
  fun w16ToW8 w16 = Word8.fromInt (Word16.toInt w16)
  fun rd (device, addr): word8 = Array.sub (device, Word16.toInt addr)
  fun rd16 (device, addr): word16 =
      Word16.orb (w8ToW16 (rd (device, addr)), Word16.<< (w8ToW16 (rd (device, addr+0wx1)), 0wx8))
  fun wr (device, addr, value): unit = Array.update (device, Word16.toInt addr, value)
  fun wr16 (device, addr, value) =
      let
        val hh = w16ToW8 (Word16.>> (value, 0w8))
        val ll = w16ToW8 value
      in
        (wr (device, addr-0w1, ll); wr (device, addr, hh))
      end
  exception MemFault
  fun read (addr: word16): word8 =
           if addr < 0wx2000 then rd (wRam, addr)
      else if addr < 0wx4000 then rd (ppuRegisters, addr-0wx2000)
      else if addr < 0wx4020 then raise MemFault
      else if addr < 0wxFFFF then rd (prgRom, addr-0wx4020)
      else raise MemFault
  fun read16 (addr: word16): word16 =
           if addr < 0wx2000 then rd16 (wRam, addr)
      else if addr < 0wx4020 then raise MemFault
      else if addr < 0wxFFFF then rd16 (prgRom, addr-0wx4020)
      else raise MemFault
  fun write (addr, value) : unit =
           if addr < 0wx2000 then wr (wRam, addr, value)
      else if addr < 0wx2008 then PPU.ioWrite (addr, value)
      else if addr < 0wx4020 then raise MemFault
      else if addr < 0wxFFFF then raise MemFault
      else raise MemFault
  fun write16 (addr, value) : unit =
           if addr < 0wx2000 then wr16 (wRam, addr, value)
      else if addr < 0wx4020 then raise MemFault
      else if addr < 0wxFFFF then raise MemFault
      else raise MemFault

  type statusRegister = {N: bool, V: bool, B5: bool, B4: bool, D: bool, I: bool, Z: bool, C: bool}
  type registers = {A: word8, X: word8, Y: word8, PC: word16, S: word8, P: statusRegister}

  datatype address =
      Implied
    | Accumulator
    | ZeroPage of word8
    | Absolute of word16
    | Relative of word8
    | Indirect of word16
    | ZeroPageIndexedX of word8
    | ZeroPageIndexedY of word8
    | AbsoluteIndexedX of word16
    | AbsoluteIndexedY of word16
    | IndexedIndirect of word8
    | IndirectIndexed of word8

  datatype operand =
      Immediate of word8
    | Address of address


  fun samePageRead16 addr =
      Word16.orb (w8ToW16 (rd (wRam, addr)), Word16.<< (w8ToW16 (rd (wRam, Word16.orb (Word16.andb (addr, 0wxff00), (addr+0wx1) mod 0wx100))), 0wx8))

  fun isCrossed (a, b) = Word16.andb (a, 0wxff00) <> Word16.andb (b, 0wxff00)

  exception InvaildAccess
  fun address (regs, ZeroPage x) = w8ToW16 x
    | address (regs, Absolute x) = x
    | address (regs, Indirect x) = samePageRead16 x
    | address (regs, ZeroPageIndexedX x) = ((w8ToW16 x) + w8ToW16 (#X regs)) mod 0wx100
    | address (regs, ZeroPageIndexedY x) = ((w8ToW16 x) + w8ToW16 (#Y regs)) mod 0wx100
    | address (regs, AbsoluteIndexedX x) = x + w8ToW16 (#X regs)
    | address (regs, IndexedIndirect x) = samePageRead16 ((w8ToW16 x + w8ToW16 (#X regs)) mod 0wx100)
    | address (regs, IndirectIndexed x) = (samePageRead16 (w8ToW16 x)) + w8ToW16 (#Y regs)
    | address _ = raise InvaildAccess
  fun conditionalAddress (regs, AbsoluteIndexedY x) = let val a = x + w8ToW16 (#Y regs) in (a, isCrossed (x, a)) end
    | conditionalAddress (regs, AbsoluteIndexedX x) = let val a = x + w8ToW16 (#X regs) in (a, isCrossed (x, a)) end
    | conditionalAddress (regs, IndirectIndexed x) = let val v = samePageRead16 (w8ToW16 x) val a = v + w8ToW16 (#Y regs) in (a, isCrossed (v, a)) end
    | conditionalAddress (regs, Relative x) = let val v = (#PC regs) + w8ToW16 x - (if x >= 0w128 then 0w256 else 0w0) in (v, isCrossed (#PC regs, v)) end
    | conditionalAddress _ = raise InvaildAccess

  fun value (regs, Immediate x) = x
    | value (regs, Address a) = read (address (regs, a))
  fun conditionalValue (regs, Address a) = let val (addr, isCrsd) = conditionalAddress (regs, a) in (read addr, isCrsd) end

  datatype instruction =
      ADC of operand
    | AND of operand
    | ASL of operand
    | BCC of address
    | BCS of address
    | BEQ of address
    | BIT of operand
    | BMI of address
    | BNE of address
    | BPL of address
    | BRK of address
    | BVC of address
    | BVS of address
    | CLC of address
    | CLD of address
    | CLI of address
    | CLV of address
    | CMP of operand
    | CPX of operand
    | CPY of operand
    | DEC of operand
    | DEX of address
    | DEY of address
    | EOR of operand
    | INC of operand
    | INX of address
    | INY of address
    | JMP of address
    | JSR of address
    | LDA of operand
    | LDX of operand
    | LDY of operand
    | LSR of operand
    | NOP of operand
    | ORA of operand
    | PHA of address
    | PHP of address
    | PLA of address
    | PLP of address
    | ROL of operand
    | ROR of operand
    | RTI of address
    | RTS of address
    | SBC of operand
    | SEC of address
    | SED of address
    | SEI of address
    | STA of address
    | STX of address
    | STY of address
    | TAX of address
    | TAY of address
    | TSX of address
    | TXA of address
    | TXS of address
    | TYA of address

  fun b2w (boolean : bool) : word8 = if boolean then 0w1 else 0w0
  fun b2i (boolean : bool) : int   = if boolean then 1 else 0
  fun w2b (word : word8) : bool = if word = 0w0 then false else true
  fun p2Word (p : statusRegister) : word8 =
        (if #N  p  then 0w128 else 0w0)
      + (if #V  p  then 0w64  else 0w0)
      + (if #B5 p  then 0w32  else 0w0)
      + (if #B4 p  then 0w16  else 0w0)
      + (if #D  p  then 0w8   else 0w0)
      + (if #I  p  then 0w4   else 0w0)
      + (if #Z  p  then 0w2   else 0w0)
      + (if #C  p  then 0w1   else 0w0)
  fun word2P (w8 : word8) : statusRegister =
      {N  = (Word8.andb (w8, 0wx80) = 0wx80),
      V  = (Word8.andb (w8, 0wx40) = 0wx40),
      B5 = (Word8.andb (w8, 0wx20) = 0wx20),
      B4 = (Word8.andb (w8, 0wx10) = 0wx10),
      D  = (Word8.andb (w8, 0wx08) = 0wx08),
      I  = (Word8.andb (w8, 0wx04) = 0wx04),
      Z  = (Word8.andb (w8, 0wx02) = 0wx02),
      C  = (Word8.andb (w8, 0wx01) = 0wx01)}
  fun checkV (A, B, C): bool = (not (w2b (Word8.>> (Word8.xorb (A, B), 0w7)))) andalso w2b (Word8.>> (Word8.xorb (A, C), 0w7))

  exception InvaildInstruction
  fun exec (regs, insn) =
      case insn of
        ADC (Address (AbsoluteIndexedX y)) => let val (v1, isCrsd) = conditionalValue (regs, (Address (AbsoluteIndexedX y))) val v2 = v1 + #A regs + b2w (#C (#P regs)) in (regs # {A = v2, P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), V = checkV (#A regs, v1, v2) ,Z = (v2=0w0), C = (v2 < #A regs)}}, b2i isCrsd) end
      | ADC (Address (AbsoluteIndexedY y)) => let val (v1, isCrsd) = conditionalValue (regs, (Address (AbsoluteIndexedY y))) val v2 = v1 + #A regs + b2w (#C (#P regs)) in (regs # {A = v2, P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), V = checkV (#A regs, v1, v2) ,Z = (v2=0w0), C = (v2 < #A regs)}}, b2i isCrsd) end
      | ADC (Address (IndirectIndexed  y)) => let val (v1, isCrsd) = conditionalValue (regs, (Address (IndirectIndexed  y))) val v2 = v1 + #A regs + b2w (#C (#P regs)) in (regs # {A = v2, P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), V = checkV (#A regs, v1, v2) ,Z = (v2=0w0), C = (v2 < #A regs)}}, b2i isCrsd) end
      | ADC x => let val v1 = value (regs, x) val v2 = v1 + #A regs + b2w (#C (#P regs)) in (regs # {A = v2, P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), V = checkV (#A regs, v1, v2) ,Z = (v2=0w0), C = (v2 < #A regs)}}, 0) end
      | AND (Address (AbsoluteIndexedX y)) => let val (v1, isCrsd) = conditionalValue (regs, (Address (AbsoluteIndexedX y))) val v2 = Word8.andb (#A regs, v1) in (regs # {A = v2, P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0)}}, b2i isCrsd) end
      | AND (Address (AbsoluteIndexedY y)) => let val (v1, isCrsd) = conditionalValue (regs, (Address (AbsoluteIndexedY y))) val v2 = Word8.andb (#A regs, v1) in (regs # {A = v2, P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0)}}, b2i isCrsd) end
      | AND (Address (IndirectIndexed  y)) => let val (v1, isCrsd) = conditionalValue (regs, (Address (IndirectIndexed  y))) val v2 = Word8.andb (#A regs, v1) in (regs # {A = v2, P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0)}}, b2i isCrsd) end
      | AND x => let val v1 = value (regs, x) val v2 = Word8.andb (#A regs, v1) in (regs # {A = v2, P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0)}}, 0) end
      | ASL (Address Accumulator) => let val v1 = (#A regs) val v2 = v1 * 0w2 in (regs # {A = v2 ,P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0), C = (Word8.andb (v1, 0wx80) = 0wx80)}}, 0) end
      | ASL (Address x) => let val v1 = value (regs, Address x) val v2 = v1 * 0w2 in (write (address (regs, x), v2); (regs # {P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0), C = (Word8.andb (v1, 0wx80) = 0wx80)}}, 0)) end
      | BCC x => if not (#C (#P regs)) then let val (addr, isCrsd) = conditionalAddress (regs, x) in (regs # {PC = addr}, 1 + b2i isCrsd) end else (regs, 0)
      | BCS x => if #C (#P regs) then let val (addr, isCrsd) = conditionalAddress (regs, x) in (regs # {PC = addr}, 1 + b2i isCrsd) end else (regs, 0)
      | BEQ x => if #Z (#P regs) then let val (addr, isCrsd) = conditionalAddress (regs, x) in (regs # {PC = addr}, 1 + b2i isCrsd) end else (regs, 0)
      | BIT x => let val v = value (regs, x) in (regs # {P = (#P regs) # {N = (Word8.andb (v, 0wx80) = 0wx80), V = (Word8.andb (v, 0wx40) = 0wx40), Z = (Word8.andb (#A regs, v) = 0w0)}}, 0) end
      | BMI x => if #N (#P regs) then let val (addr, isCrsd) = conditionalAddress (regs, x) in (regs # {PC = addr}, 1 + b2i isCrsd) end else (regs, 0)
      | BNE x => if not (#Z (#P regs)) then let val (addr, isCrsd) = conditionalAddress (regs, x) in (regs # {PC = addr}, 1 + b2i isCrsd) end else (regs, 0)
      | BPL x => if not (#N (#P regs)) then let val (addr, isCrsd) = conditionalAddress (regs, x) in (regs # {PC = addr}, 1 + b2i isCrsd) end else (regs, 0)
      | BRK x => (write16 (0wx100 + w8ToW16 (#S regs), 0w1 + #PC regs); write (0wx100+w8ToW16 ((#S regs) - 0w2), p2Word (#P regs)); (regs # {S = (#S regs)-0w3, PC = read16 0wxFFFE, P = (#P regs) # {B4=true, I=true}}, 0))
      | BVC x => if not (#V (#P regs)) then let val (addr, isCrsd) = conditionalAddress (regs, x) in (regs # {PC = address (regs, x)}, 1 + b2i isCrsd) end else (regs, 0)
      | BVS x => if #V (#P regs) then let val (addr, isCrsd) = conditionalAddress (regs, x) in (regs # {PC = address (regs, x)}, 1 + b2i isCrsd) end else (regs, 0)
      | CLC x => (regs # {P = (#P regs) # {C=false}}, 0)
      | CLD x => (regs # {P = (#P regs) # {D=false}}, 0)
      | CLI x => (regs # {P = (#P regs) # {I=false}}, 0)
      | CLV x => (regs # {P = (#P regs) # {V=false}}, 0)
      | CMP (Address (AbsoluteIndexedX y)) => let val (v1, isCrsd) = conditionalValue (regs, (Address (AbsoluteIndexedX y))) val v2 = #A regs - v1 in (regs # {P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0), C = not (v2 > #A regs)}}, b2i isCrsd) end
      | CMP (Address (AbsoluteIndexedY y)) => let val (v1, isCrsd) = conditionalValue (regs, (Address (AbsoluteIndexedY y))) val v2 = #A regs - v1 in (regs # {P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0), C = not (v2 > #A regs)}}, b2i isCrsd) end
      | CMP (Address (IndirectIndexed  y)) => let val (v1, isCrsd) = conditionalValue (regs, (Address (IndirectIndexed  y))) val v2 = #A regs - v1 in (regs # {P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0), C = not (v2 > #A regs)}}, b2i isCrsd) end
      | CMP x => let val v1 = value (regs, x) val v2 = #A regs - v1 in (regs # {P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0), C = not (v2 > #A regs)}}, 0) end
      | CPX x => let val v1 = value (regs, x) val v2 = #X regs - v1 in (regs # {P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0), C = not (v2 > #X regs)}}, 0) end
      | CPY x => let val v1 = value (regs, x) val v2 = #Y regs - v1 in (regs # {P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0), C = not (v2 > #Y regs)}}, 0) end
      | DEC (Address x) => let val v1 = value (regs, Address x) val v2 = v1 - 0w1 in (write (address (regs, x), v2); (regs # {P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0)}}, 0)) end
      | DEX x => let val v = #X regs - 0w1 in (regs # {X = v, P = (#P regs) # {N = (Word8.andb (v, 0wx80) = 0wx80), Z = (v=0w0)}}, 0) end
      | DEY x => let val v = #Y regs - 0w1 in (regs # {Y = v, P = (#P regs) # {N = (Word8.andb (v, 0wx80) = 0wx80), Z = (v=0w0)}}, 0) end
      | EOR (Address (AbsoluteIndexedX y)) => let val (v1, isCrsd) = conditionalValue (regs, (Address (AbsoluteIndexedX y))) val v2 = Word8.xorb (#A regs, v1) in (regs # {A = v2, P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0)}}, b2i isCrsd) end
      | EOR (Address (AbsoluteIndexedY y)) => let val (v1, isCrsd) = conditionalValue (regs, (Address (AbsoluteIndexedY y))) val v2 = Word8.xorb (#A regs, v1) in (regs # {A = v2, P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0)}}, b2i isCrsd) end
      | EOR (Address (IndirectIndexed  y)) => let val (v1, isCrsd) = conditionalValue (regs, (Address (IndirectIndexed  y))) val v2 = Word8.xorb (#A regs, v1) in (regs # {A = v2, P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0)}}, b2i isCrsd) end
      | EOR x => let val v1 = value (regs, x) val v2 = Word8.xorb (#A regs, v1) in (regs # {A = v2, P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0)}}, 0) end
      | INC (Address x) => let val v1 = value (regs, Address x) val v2 = v1 + 0w1 in (write (address (regs, x), v2); (regs # {P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0)}}, 0)) end
      | INX x => let val v = #X regs + 0w1 in (regs # {X = v, P = (#P regs) # {N = (Word8.andb (v, 0wx80) = 0wx80), Z = (v=0w0)}}, 0) end
      | INY x => let val v = #Y regs + 0w1 in (regs # {Y = v, P = (#P regs) # {N = (Word8.andb (v, 0wx80) = 0wx80), Z = (v=0w0)}}, 0) end
      | JMP x => (regs # {PC = address (regs, x)}, 0)
      | JSR x => (write16 (0wx100+w8ToW16 (#S regs), (#PC regs) - 0w1); (regs # {S = (#S regs)-0w2, PC = address (regs, x)}, 0))
      | LDA (Address (AbsoluteIndexedX y)) => let val (v, isCrsd) = conditionalValue (regs, (Address (AbsoluteIndexedX y))) in (regs # {A = v, P = (#P regs) # {N = (Word8.andb (v, 0wx80) = 0wx80), Z = (v=0w0)}}, b2i isCrsd) end
      | LDA (Address (AbsoluteIndexedY y)) => let val (v, isCrsd) = conditionalValue (regs, (Address (AbsoluteIndexedY y))) in (regs # {A = v, P = (#P regs) # {N = (Word8.andb (v, 0wx80) = 0wx80), Z = (v=0w0)}}, b2i isCrsd) end
      | LDA (Address (IndirectIndexed  y)) => let val (v, isCrsd) = conditionalValue (regs, (Address (IndirectIndexed  y))) in (regs # {A = v, P = (#P regs) # {N = (Word8.andb (v, 0wx80) = 0wx80), Z = (v=0w0)}}, b2i isCrsd) end
      | LDA x => let val v = value (regs, x) in (regs # {A = v, P = (#P regs) # {N = (Word8.andb (v, 0wx80) = 0wx80), Z = (v=0w0)}}, 0) end
      | LDX (Address (AbsoluteIndexedY y)) => let val (v, isCrsd) = conditionalValue (regs, (Address (AbsoluteIndexedY y))) in (regs # {X = v, P = (#P regs) # {N = (Word8.andb (v, 0wx80) = 0wx80), Z = (v=0w0)}}, b2i isCrsd) end
      | LDX x => let val v = value (regs, x) in (regs # {X = v, P = (#P regs) # {N = (Word8.andb (v, 0wx80) = 0wx80), Z = (v=0w0)}}, 0) end
      | LDY (Address (AbsoluteIndexedX y)) => let val (v, isCrsd) = conditionalValue (regs, (Address (AbsoluteIndexedX y))) in (regs # {Y = v, P = (#P regs) # {N = (Word8.andb (v, 0wx80) = 0wx80), Z = (v=0w0)}}, b2i isCrsd) end
      | LDY x => let val v = value (regs, x) in (regs # {Y = v, P = (#P regs) # {N = (Word8.andb (v, 0wx80) = 0wx80), Z = (v=0w0)}}, 0) end
      | LSR (Address Accumulator) => let val v1 = (#A regs) val v2 = v1 div 0w2 in (regs # {A = v2 ,P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0), C = (Word8.andb (v1, 0w1) = 0w1)}},0) end
      | LSR (Address x) => let val v1 = value (regs, Address x) val v2 = v1 div 0w2 in (write (address (regs, x), v2); (regs # {P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0), C = (Word8.andb (v1, 0w1) = 0w1)}},0)) end
      | NOP (Address (AbsoluteIndexedX y)) => let val (_, isCrsd) = conditionalAddress (regs, AbsoluteIndexedX y) in (regs, b2i isCrsd) end
      | NOP x => (regs, 0)
      | ORA (Address (AbsoluteIndexedX y)) => let val (v1, isCrsd) = conditionalValue (regs, (Address (AbsoluteIndexedX y))) val v2 = Word8.orb (#A regs, v1) in (regs # {A = v2, P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0)}}, b2i isCrsd) end
      | ORA (Address (AbsoluteIndexedY y)) => let val (v1, isCrsd) = conditionalValue (regs, (Address (AbsoluteIndexedY y))) val v2 = Word8.orb (#A regs, v1) in (regs # {A = v2, P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0)}}, b2i isCrsd) end
      | ORA (Address (IndirectIndexed  y)) => let val (v1, isCrsd) = conditionalValue (regs, (Address (IndirectIndexed  y))) val v2 = Word8.orb (#A regs, v1) in (regs # {A = v2, P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0)}}, b2i isCrsd) end
      | ORA x => let val v1 = value (regs, x) val v2 = Word8.orb (#A regs, v1) in (regs # {A = v2, P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0)}}, 0) end
      | PHA x => (write (0wx100+w8ToW16 (#S regs), #A regs); (regs # {S = (#S regs)-0w1}, 0))
      | PHP x => (write (0wx100+w8ToW16 (#S regs), Word8.orb (p2Word (#P regs), 0wx10)); (regs # {S = (#S regs)-0w1}, 0))
      | PLA x => let val v =  read (0wx100 + w8ToW16 ((#S regs) + 0w1)) in (regs # {A = v, S = (#S regs) + 0w1, P = (#P regs) # {N = (Word8.andb (v, 0wx80) = 0wx80), Z = (v=0w0)}}, 0) end
      | PLP x => let val v =  Word8.orb (Word8.andb (read (0wx100 + w8ToW16 ((#S regs) + 0w1)), 0wxCF), Word8.andb (p2Word (#P regs), 0wx30)) in (regs # {S = (#S regs) + 0w1, P = word2P v}, 0) end
      | ROL (Address Accumulator) => let val v1 = (#A regs) val v2 = v1 * 0w2 + b2w (#C (#P regs)) in (regs # {A = v2 ,P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0), C = (Word8.andb (v1, 0wx80) = 0wx80)}}, 0) end
      | ROL (Address x) => let val v1 = value (regs, Address x) val v2 = v1 * 0w2 + b2w (#C (#P regs)) in (write (address (regs, x), v2); (regs # {P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0), C = (Word8.andb (v1, 0wx80) = 0wx80)}}, 0)) end
      | ROR (Address Accumulator) => let val v1 = (#A regs) val v2 = v1 div 0w2 + 0w128 * b2w (#C (#P regs)) in (regs # {A = v2 ,P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0), C = (Word8.andb (v1, 0w1) = 0w1)}}, 0) end
      | ROR (Address x) => let val v1 = value (regs, Address x) val v2 = v1 div 0w2 + 0w128 * b2w (#C (#P regs)) in (write (address (regs, x), v2); (regs # {P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0), C = (Word8.andb (v1, 0w1) = 0w1)}}, 0)) end
      | RTI x => let val v1 =  Word8.orb (Word8.andb (read (0wx100 + w8ToW16 ((#S regs) + 0w1)), 0wxCF), Word8.andb (p2Word (#P regs), 0wx30)) val v2 = read16 (0wx100 + w8ToW16 ((#S regs) + 0w2)) in (regs # {PC = v2, S = (#S regs) + 0w3, P = word2P v1}, 0) end
      | RTS x => (regs # {PC = 0w1 + read16 (0wx100 + w8ToW16 ((#S regs) + 0w1)), S = (#S regs) + 0w2}, 0)
      | SBC (Address (AbsoluteIndexedX y)) => let val (v, isCrsd) = conditionalValue (regs, (Address (AbsoluteIndexedX y))) val v1 = 0w1 + Word8.notb v - 0w1 + b2w (#C (#P regs)) val v2 = v1 + #A regs in (regs # {A = v2, P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), V = checkV (#A regs, v1, v2) ,Z = (v2=0w0), C = (v2 < #A regs)}}, b2i isCrsd) end
      | SBC (Address (AbsoluteIndexedY y)) => let val (v, isCrsd) = conditionalValue (regs, (Address (AbsoluteIndexedY y))) val v1 = 0w1 + Word8.notb v - 0w1 + b2w (#C (#P regs)) val v2 = v1 + #A regs in (regs # {A = v2, P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), V = checkV (#A regs, v1, v2) ,Z = (v2=0w0), C = (v2 < #A regs)}}, b2i isCrsd) end
      | SBC (Address (IndirectIndexed  y)) => let val (v, isCrsd) = conditionalValue (regs, (Address (IndirectIndexed  y))) val v1 = 0w1 + Word8.notb v - 0w1 + b2w (#C (#P regs)) val v2 = v1 + #A regs in (regs # {A = v2, P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), V = checkV (#A regs, v1, v2) ,Z = (v2=0w0), C = (v2 < #A regs)}}, b2i isCrsd) end
      | SBC x => let val v1 = 0w1 + Word8.notb (value (regs, x))  - 0w1 + b2w (#C (#P regs)) val v2 = v1 + #A regs in (regs # {A = v2, P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), V = checkV (#A regs, v1, v2) ,Z = (v2=0w0), C = (v2 < #A regs)}}, 0) end
      | SEC x => (regs # {P = (#P regs) # {C = true}}, 0)
      | SED x => (regs # {P = (#P regs) # {D = true}}, 0)
      | SEI x => (regs # {P = (#P regs) # {I = true}}, 0)
      | STA x => (write (address (regs, x), #A regs); (regs, 0))
      | STX x => (write (address (regs, x), #X regs); (regs, 0))
      | STY x => (write (address (regs, x), #Y regs); (regs, 0))
      | TAX x => (regs # {X = #A regs, P = (#P regs) # {N = (Word8.andb (#A regs, 0wx80) = 0wx80), Z = (#A regs = 0w0)}}, 0)
      | TAY x => (regs # {Y = #A regs, P = (#P regs) # {N = (Word8.andb (#A regs, 0wx80) = 0wx80), Z = (#A regs = 0w0)}}, 0)
      | TSX x => (regs # {X = #S regs, P = (#P regs) # {N = (Word8.andb (#S regs, 0wx80) = 0wx80), Z = (#S regs = 0w0)}}, 0)
      | TXA x => (regs # {A = #X regs, P = (#P regs) # {N = (Word8.andb (#X regs, 0wx80) = 0wx80), Z = (#X regs = 0w0)}}, 0)
      | TXS x => (regs # {S = #X regs}, 0)
      | TYA x => (regs # {A = #Y regs, P = (#P regs) # {N = (Word8.andb (#Y regs, 0wx80) = 0wx80), Z = (#Y regs = 0w0)}}, 0)
      | _     => raise InvaildInstruction

  exception FailedDecode
  fun fetchAndDecode (pc: word16): instruction * word16 * int =
      let
        fun imm  _ = Immediate (read (pc + 0w1))
        fun zp   _ = ZeroPage (read (pc + 0w1))
        fun zpx  _ = ZeroPageIndexedX (read (pc + 0w1))
        fun zpy  _ = ZeroPageIndexedY (read (pc + 0w1))
        fun izx  _ = IndexedIndirect (read (pc + 0w1))
        fun izy  _ = IndirectIndexed (read (pc + 0w1))
        fun abs  _ = Absolute (read16 (pc + 0w1))
        fun abx  _ = AbsoluteIndexedX (read16 (pc + 0w1))
        fun aby  _ = AbsoluteIndexedY (read16 (pc + 0w1))
        fun ind  _ = Indirect (read16 (pc + 0w1))
        fun rel  _ = Relative (read (pc + 0w1))
      in
        case read pc of
          0wx00 => (BRK Implied, pc + 0w1, 7)
        | 0wx01 => (ORA (Address (IndexedIndirect (read (pc + 0w1)))), pc + 0w2, 6)
        | 0wx05 => (ORA (Address (ZeroPage (read (pc + 0w1)))), pc + 0w2, 3)
        | 0wx06 => (ASL (Address (ZeroPage (read (pc + 0w1)))), pc + 0w2, 5)
        | 0wx08 => (PHP Implied, pc + 0w1, 3)
        | 0wx09 => (ORA (Immediate (read (pc + 0w1))), pc + 0w2, 2)
        | 0wx0A => (ASL (Address Accumulator), pc + 0w1, 2)
        | 0wx0D => (ORA (Address (Absolute (read16 (pc + 0w1)))), pc + 0w3, 4)
        | 0wx0E => (ASL (Address (Absolute (read16 (pc + 0w1)))), pc + 0w3, 6)
        | 0wx10 => (BPL (Relative (read (pc + 0w1))), pc + 0w2, 2)
        | 0wx11 => (ORA (Address (IndirectIndexed (read (pc + 0w1)))), pc + 0w2, 5)
        | 0wx15 => (ORA (Address (ZeroPageIndexedX (read (pc + 0w1)))), pc + 0w2, 4)
        | 0wx16 => (ASL (Address (ZeroPageIndexedX (read (pc + 0w1)))), pc + 0w2, 6)
        | 0wx18 => (CLC Implied, pc + 0w1, 2)
        | 0wx19 => (ORA (Address (AbsoluteIndexedY (read16 (pc + 0w1)))), pc + 0w3, 4)
        | 0wx1D => (ORA (Address (AbsoluteIndexedX (read16 (pc + 0w1)))), pc + 0w3, 4)
        | 0wx1E => (ASL (Address (AbsoluteIndexedX (read16 (pc + 0w1)))), pc + 0w3, 7)
        | 0wx20 => (JSR (Absolute (read16 (pc + 0w1))), pc + 0w3, 6)
        | 0wx21 => (AND (Address (IndexedIndirect (read (pc + 0w1)))), pc + 0w2, 6)
        | 0wx24 => (BIT (Address (zp ())), pc + 0w2, 3)
        | 0wx25 => (AND (Address (zp ())), pc + 0w2, 3)
        | 0wx26 => (ROL (Address (zp ())), pc + 0w2, 5)
        | 0wx28 => (PLP Implied, pc + 0w1, 4)
        | 0wx29 => (AND (imm ()), pc + 0w2, 2)
        | 0wx2A => (ROL (Address Accumulator), pc + 0w1, 2)
        | 0wx2C => (BIT (Address (abs ())), pc + 0w3, 4)
        | 0wx2D => (AND (Address (abs ())), pc + 0w3, 4)
        | 0wx2E => (ROL (Address (abs ())), pc + 0w3, 6)
        | 0wx30 => (BMI (rel ()), pc + 0w2, 2)
        | 0wx31 => (AND (Address (izy ())), pc + 0w2, 5)
        | 0wx35 => (AND (Address (zpx ())), pc + 0w2, 4)
        | 0wx36 => (ROL (Address (zpx ())), pc + 0w2, 6)
        | 0wx38 => (SEC Implied, pc + 0w1, 2)
        | 0wx39 => (AND (Address (aby ())), pc + 0w3, 4)
        | 0wx3D => (AND (Address (abx ())), pc + 0w3, 4)
        | 0wx3E => (ROL (Address (abx ())), pc + 0w3, 7)
        | 0wx40 => (RTI Implied, pc + 0w1, 6)
        | 0wx41 => (EOR (Address (izx ())), pc + 0w2, 6)
        | 0wx45 => (EOR (Address (zp ())), pc + 0w2, 3)
        | 0wx46 => (LSR (Address (zp ())), pc + 0w2, 5)
        | 0wx48 => (PHA Implied, pc + 0w1, 3)
        | 0wx49 => (EOR  (imm ()), pc + 0w2, 2)
        | 0wx4A => (LSR (Address Accumulator), pc + 0w1, 2)
        | 0wx4C => (JMP (Absolute (read16 (pc + 0w1))), pc + 0w3, 3)
        | 0wx4D => (EOR (Address (abs ())), pc + 0w3, 4)
        | 0wx4E => (LSR (Address (abs ())), pc + 0w3, 6)
        | 0wx50 => (BVC (rel ()), pc + 0w2, 2)
        | 0wx51 => (EOR (Address (izy ())), pc + 0w2, 5)
        | 0wx55 => (EOR (Address (zpx ())), pc + 0w2, 4)
        | 0wx56 => (LSR (Address (zpx ())), pc + 0w2, 6)
        | 0wx58 => (CLI Implied, pc + 0w1, 2)
        | 0wx59 => (EOR (Address (aby ())), pc + 0w3, 4)
        | 0wx5D => (EOR (Address (abx ())), pc + 0w3, 4)
        | 0wx5E => (LSR (Address (abx ())), pc + 0w3, 7)
        | 0wx60 => (RTS Implied, pc + 0w1, 6)
        | 0wx61 => (ADC (Address (izx ())), pc + 0w2, 6)
        | 0wx65 => (ADC (Address (zp ())), pc + 0w2, 3)
        | 0wx66 => (ROR (Address (zp ())), pc + 0w2, 5)
        | 0wx68 => (PLA Implied, pc + 0w1, 4)
        | 0wx69 => (ADC (imm ()), pc + 0w2, 2)
        | 0wx6A => (ROR (Address Accumulator), pc + 0w1, 2)
        | 0wx6C => (JMP (ind ()), pc + 0w3, 5)
        | 0wx6D => (ADC (Address (abs ())), pc + 0w3, 4)
        | 0wx6E => (ROR (Address (abs ())), pc + 0w3, 6)
        | 0wx70 => (BVS (rel ()), pc + 0w2, 2)
        | 0wx71 => (ADC (Address (izy ())), pc + 0w2, 5)
        | 0wx75 => (ADC (Address (zpx ())), pc + 0w2, 4)
        | 0wx76 => (ROR (Address (zpx ())), pc + 0w2, 6)
        | 0wx78 => (SEI Implied, pc + 0w1, 2)
        | 0wx79 => (ADC (Address (aby ())), pc + 0w3, 4)
        | 0wx7D => (ADC (Address (abx ())), pc + 0w3, 4)
        | 0wx7E => (ROR (Address (abx ())), pc + 0w3, 7)
        | 0wx81 => (STA (izx ()), pc + 0w2, 6)
        | 0wx84 => (STY (zp ()), pc + 0w2, 3)
        | 0wx85 => (STA (zp ()), pc + 0w2, 3)
        | 0wx86 => (STX (ZeroPage (read (pc + 0w1))), pc + 0w2, 3)
        | 0wx88 => (DEY Implied, pc + 0w1, 2)
        | 0wx8A => (TXA Implied, pc + 0w1, 2)
        | 0wx8C => (STY (abs ()), pc + 0w3, 4)
        | 0wx8D => (STA (abs ()), pc + 0w3, 4)
        | 0wx8E => (STX (abs ()), pc + 0w3, 4)
        | 0wx90 => (BCC (rel ()), pc + 0w2, 2)
        | 0wx91 => (STA (izy ()), pc + 0w2, 6)
        | 0wx94 => (STY (zpx ()), pc + 0w2, 4)
        | 0wx95 => (STA (zpx ()), pc + 0w2, 4)
        | 0wx96 => (STX (zpy ()), pc + 0w2, 4)
        | 0wx98 => (TYA Implied, pc + 0w1, 2)
        | 0wx99 => (STA (aby ()), pc + 0w3, 5)
        | 0wx9A => (TXS Implied, pc + 0w1, 2)
        | 0wx9D => (STA (abx ()), pc + 0w3, 5)
        | 0wxA0 => (LDY (imm ()), pc + 0w2, 2)
        | 0wxA1 => (LDA (Address (izx ())), pc + 0w2, 6)
        | 0wxA2 => (LDX (Immediate (read (pc + 0w1))), pc + 0w2, 2)
        | 0wxA4 => (LDY (Address (zp ())), pc + 0w2, 3)
        | 0wxA5 => (LDA (Address (ZeroPage (read (pc + 0w1)))), pc + 0w2, 3)
        | 0wxA6 => (LDX (Address (zp ())), pc + 0w2, 3)
        | 0wxA8 => (TAY Implied, pc + 0w1, 2)
        | 0wxA9 => (LDA (Immediate (read (pc + 0w1))), pc + 0w2, 2)
        | 0wxAA => (TAX Implied, pc + 0w1, 2)
        | 0wxAC => (LDY (Address (abs ())), pc + 0w3, 4)
        | 0wxAD => (LDA (Address (Absolute (read16 (pc + 0w1)))), pc + 0w3, 4)
        | 0wxAE => (LDX (Address (abs ())), pc + 0w3, 4)
        | 0wxB0 => (BCS (rel ()), pc + 0w2, 2)
        | 0wxB1 => (LDA (Address (izy ())), pc + 0w2, 5)
        | 0wxB4 => (LDY (Address (zpx ())), pc + 0w2, 4)
        | 0wxB5 => (LDA (Address (zpx ())), pc + 0w2, 4)
        | 0wxB6 => (LDX (Address (zpy ())), pc + 0w2, 4)
        | 0wxB8 => (CLV Implied, pc + 0w1, 2)
        | 0wxB9 => (LDA (Address (aby ())), pc + 0w3, 4)
        | 0wxBA => (TSX Implied, pc + 0w1, 2)
        | 0wxBC => (LDY (Address (abx ())), pc + 0w3, 4)
        | 0wxBD => (LDA (Address (abx ())), pc + 0w3, 4)
        | 0wxBE => (LDX (Address (aby ())), pc + 0w3, 4)
        | 0wxC0 => (CPY (imm ()), pc + 0w2, 2)
        | 0wxC1 => (CMP (Address (izx ())), pc + 0w2, 6)
        | 0wxC4 => (CPY (Address (zp ())), pc + 0w2, 3)
        | 0wxC5 => (CMP (Address (zp ())), pc + 0w2, 3)
        | 0wxC6 => (DEC (Address (zp ())), pc + 0w2, 5)
        | 0wxC8 => (INY Implied, pc + 0w1, 2)
        | 0wxC9 => (CMP (imm ()), pc + 0w2, 2)
        | 0wxCA => (DEX Implied, pc + 0w1, 2)
        | 0wxCC => (CPY (Address (abs ())), pc + 0w3, 4)
        | 0wxCD => (CMP (Address (abs ())), pc + 0w3, 4)
        | 0wxCE => (DEC (Address (abs ())), pc + 0w3, 6)
        | 0wxD0 => (BNE (rel ()), pc + 0w2, 2)
        | 0wxD1 => (CMP (Address (izy ())), pc + 0w2, 5)
        | 0wxD5 => (CMP (Address (zpx ())), pc + 0w2, 4)
        | 0wxD6 => (DEC (Address (zpx ())), pc + 0w2, 6)
        | 0wxD8 => (CLD Implied, pc + 0w1, 2)
        | 0wxD9 => (CMP (Address (aby ())), pc + 0w3, 4)
        | 0wxDD => (CMP (Address (abx ())), pc + 0w3, 4)
        | 0wxDE => (DEC (Address (abx ())), pc + 0w3, 7)
        | 0wxE0 => (CPX (imm ()), pc + 0w2, 2)
        | 0wxE1 => (SBC (Address (izx ())), pc + 0w2, 6)
        | 0wxE4 => (CPX (Address (zp ())), pc + 0w2, 3)
        | 0wxE5 => (SBC (Address (zp ())), pc + 0w2, 3)
        | 0wxE6 => (INC (Address (zp ())), pc + 0w2, 5)
        | 0wxE8 => (INX Implied, pc + 0w1, 2)
        | 0wxE9 => (SBC (imm ()), pc + 0w2, 2)
        | 0wxEA => (NOP (Address Implied), pc + 0w1, 2)
        | 0wxEC => (CPX (Address (abs ())), pc + 0w3, 4)
        | 0wxED => (SBC (Address (abs ())), pc + 0w3, 4)
        | 0wxEE => (INC (Address (abs ())), pc + 0w3, 6)
        | 0wxF0 => (BEQ (rel ()), pc + 0w2, 2)
        | 0wxF1 => (SBC (Address (izy ())), pc + 0w2, 5)
        | 0wxF5 => (SBC (Address (zpx ())), pc + 0w2, 4)
        | 0wxF6 => (INC (Address (zpx ())), pc + 0w2, 6)
        | 0wxF8 => (SED Implied, pc + 0w1, 2)
        | 0wxF9 => (SBC (Address (aby ())), pc + 0w3, 4)
        | 0wxFD => (SBC (Address (abx ())), pc + 0w3, 4)
        | 0wxFE => (INC (Address (abx ())), pc + 0w3, 7)
        (* some illegal opcodes for an test *)
        | 0wx04 => (NOP (Address (zp())), pc + 0w2, 3)
        | 0wx44 => (NOP (Address (zp())), pc + 0w2, 3)
        | 0wx64 => (NOP (Address (zp())), pc + 0w2, 3)
        | 0wx0C => (NOP (Address (abs())), pc + 0w3, 4)
        | 0wx14 => (NOP (Address (zpx())), pc + 0w2, 4)
        | 0wx34 => (NOP (Address (zpx())), pc + 0w2, 4)
        | 0wx54 => (NOP (Address (zpx())), pc + 0w2, 4)
        | 0wx74 => (NOP (Address (zpx())), pc + 0w2, 4)
        | 0wxD4 => (NOP (Address (zpx())), pc + 0w2, 4)
        | 0wxF4 => (NOP (Address (zpx())), pc + 0w2, 4)
        | 0wx1A => (NOP (Address Implied), pc + 0w1, 2)
        | 0wx3A => (NOP (Address Implied), pc + 0w1, 2)
        | 0wx5A => (NOP (Address Implied), pc + 0w1, 2)
        | 0wx7A => (NOP (Address Implied), pc + 0w1, 2)
        | 0wxDA => (NOP (Address Implied), pc + 0w1, 2)
        | 0wxFA => (NOP (Address Implied), pc + 0w1, 2)
        | 0wx80 => (NOP (imm ()), pc + 0w2, 2)
        | 0wx1C => (NOP (Address (abx ())), pc + 0w3, 4)
        | 0wx3C => (NOP (Address (abx ())), pc + 0w3, 4)
        | 0wx5C => (NOP (Address (abx ())), pc + 0w3, 4)
        | 0wx7C => (NOP (Address (abx ())), pc + 0w3, 4)
        | 0wxDC => (NOP (Address (abx ())), pc + 0w3, 4)
        | 0wxFC => (NOP (Address (abx ())), pc + 0w3, 4)
        | _     =>  (print (Word16.toString pc ^ ":"
                  ^ Word8.toString (read pc) ^ " "
                  ^ Word8.toString (read (0w1 + pc)) ^ " "
                  ^ Word8.toString (read (0w2 + pc)) ^ "\n");
                  raise FailedDecode)
      end

  fun alignDigit w = (if 0wx0 <= w andalso w <= 0wxF then "0" else "") ^ Word8.toString w
  fun prtRegs (regs, cycle) =
      print ( Word16.toString (#PC regs)
            ^ ( ":" ^ Word8.toString (read (#PC regs)) ^ " " ^ Word8.toString (read (0w1 + #PC regs)) ^ " " ^ Word8.toString (read (0w2 + #PC regs)))
            ^ " A:"  ^ alignDigit (#A regs)
            ^ " X:"  ^ alignDigit (#X regs)
            ^ " Y:"  ^ alignDigit (#Y regs)
            ^ " P:"  ^ Word8.toString (p2Word (#P regs))
            ^ " SP:" ^ Word8.toString (#S regs)
            ^ " CYC:" ^ Int.toString cycle
            ^ "\n")

  fun cpu (regs, cycle) =
      let
        val (insn, nextPc, baseCycle) = fetchAndDecode (#PC regs)
        val (newRegs, additionalCycle) = exec (regs # {PC = nextPc}, insn)
      in
        (* (prtRegs (regs, cycle);  *)
        (newRegs, cycle + baseCycle + additionalCycle)
        (* ) *)
      end
end