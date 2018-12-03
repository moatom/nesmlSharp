val wram : word8 array = Array.array (4 * Word.toInt 0wx800, 0wx0: word8)
val cartridge : word8 array = Array.array (Word.toInt 0wxBFE0, 0wx0: word8)

(* val file = BinIO.openIn "./sample1/sample1.nes" *)
val file = BinIO.openIn "./sample1/nestest.nes"
val prgrom  : word8 vector = BinIO.inputAll file
;BinIO.closeIn file;

(* NROM-128のロード *)
val romsize = Word8.toInt (Vector.sub (prgrom, 4)) * 16384
fun mapper0 16384 = ()
  | mapper0 n =
    let
      val subed =  Vector.sub (prgrom, n+16)
      val target = n + Word.toInt (0wx8000-0wx4020)
    in
      (Array.update (cartridge, target, subed);
       Array.update (cartridge, target + 16384, subed);
       mapper0 (n+1))
    end
;if romsize = 16384 then mapper0 0 else ();


fun w8ToW16 w8 = Word16.fromInt (Word8.toInt w8)
fun w16ToW8 w16 = Word8.fromInt (Word16.toInt w16)
fun rd (device, addr) = Array.sub (device, Word16.toInt addr)
fun rd16 (device, addr) =
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
         if addr < 0wx2000 then rd (wram, addr)
    else if addr < 0wx4020 then raise MemFault
    else if addr < 0wxFFFF then rd (cartridge, addr-0wx4020)
    else raise MemFault
fun read16 (addr: word16): word16 =
         if addr < 0wx2000 then rd16 (wram, addr)
    else if addr < 0wx4020 then raise MemFault
    else if addr < 0wxFFFF then rd16 (cartridge, addr-0wx4020)
    else raise MemFault
fun write (addr, value) : unit =
         if addr < 0wx2000 then wr (wram, addr, value)
    else if addr < 0wx4020 then raise MemFault
    else if addr < 0wxFFFF then raise MemFault
    else raise MemFault
fun write16 (addr, value) : unit =
         if addr < 0wx2000 then wr16 (wram, addr, value)
    else if addr < 0wx4020 then raise MemFault
    else if addr < 0wxFFFF then raise MemFault
    else raise MemFault

(* type registers = {A: word8, X: word8, Y: word8, PC: word16, S: word8, P: word8} *)
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

exception InvaildAccess
(* val address : registers * address -> word16 *)
fun address (regs, ZeroPage x) = w8ToW16 x
  | address (regs, Absolute x) = x
  | address (regs, Relative x) = (#PC regs) + w8ToW16 x
  | address (regs, Indirect x) = w8ToW16 (read x)
  | address (regs, ZeroPageIndexedX x) = (w8ToW16 x) + w8ToW16 (#X regs)
  | address (regs, ZeroPageIndexedY x) = (w8ToW16 x) + w8ToW16 (#Y regs)
  | address (regs, AbsoluteIndexedX x) = x + w8ToW16 (#X regs)
  | address (regs, AbsoluteIndexedY x) = x + w8ToW16 (#Y regs)
  | address (regs, IndexedIndirect x) = read16 (w8ToW16 (x + (#X regs)))
  | address (regs, IndirectIndexed x) = (read16 (w8ToW16 x)) + w8ToW16 (#Y regs)
  | address _ = raise InvaildAccess

fun value (regs, Immediate x) = x
  | value (regs, Address a) = read (address (regs, a))

(* valueしたいかどうか *)
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
  | DEC of address
  | DEX of address
  | DEY of address
  | EOR of operand
  | INC of address
  | INX of address
  | INY of address
  | JMP of address
  | JSR of address
  | LDA of operand
  | LDX of operand
  | LDY of operand
  | LSR of address
  | NOP of address
  | ORA of operand
  | PHA of address
  | PHP of address
  | PLA of address
  | PLP of address
  | ROL of address
  | ROR of address
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


(* val exec : registers * instruction -> registers *)
fun exec (regs, insn) =
    case insn of
      ADC x => let val v1 = value (regs, x) val v2 = v1 + #A regs in regs # {A = v2, P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0), C = (Word8.andb (v1, 0wx80) = 0wx80)}} end
    | AND x => let val v1 = value (regs, x) val v2 = Word8.andb (#A regs, v1) in regs # {A = v2, P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0)}} end
    | ASL (Address x) => let val v1 = value (regs, Address x) val v2 = v1 * 0w2 in (write (address (regs, x), v2); regs # {P = (#P regs) # {N = (Word8.andb (v2, 0wx80) = 0wx80), Z = (v2=0w0), C = (Word8.andb (v1, 0wx80) = 0wx80)}}) end
    | BCC x => (if not (#C (#P regs)) then regs # {PC = address (regs, x)} else regs)
    | BCS x => (if #C (#P regs) then regs # {PC = address (regs, x)} else regs)
    | BEQ x => (if #Z (#P regs) then regs # {PC = address (regs, x)} else regs)
    | BIT x => let val v = value (regs, x) in regs # {P = (#P regs) # {N = (Word8.andb (v, 0wx80) = 0wx80), V = (Word8.andb (v, 0wx40) = 0wx40), Z = (Word8.andb (#A regs, v) = 0w0)}} end
    | BMI x => (if #N (#P regs) then regs # {PC = address (regs, x)} else regs)
    | BNE x => (if not (#Z (#P regs)) then regs # {PC = address (regs, x)} else regs)
    | BPL x => (if not (#N (#P regs)) then regs # {PC = address (regs, x)} else regs)
    | BRK x => (write16 (0wx100+w8ToW16 (#S regs), #PC regs); regs # {S = (#S regs)-0w1, PC = read16 0wxFFFE, P = (#P regs) # {B4=true, I=true}})
    | BVC x => (if not (#V (#P regs)) then regs # {PC = address (regs, x)} else regs)
    | BVS x => (if #V (#P regs) then regs # {PC = address (regs, x)} else regs)
    | CLC x => (regs # {P = (#P regs) # {C=false}})
    | CLD x => regs
    | CLI x => regs
    | CLV x => regs
    | CMP x => regs
    | CPX x => regs
    | CPY x => regs
    | DEC x => regs
    | DEX x => regs
    | DEY x => regs
    | EOR x => regs
    | INC x => regs
    | INX x => regs
    | INY x => regs
    | JMP x => (regs # {PC = address (regs, x)})
    | JSR x => (write16 (0wx100+w8ToW16 (#S regs), #PC regs); regs # {S = (#S regs)-0w2, PC = address (regs, x)})
    | LDA x => let val v = value (regs, x) in regs # {A = v, P = (#P regs) # {N = (Word8.andb (v, 0wx80) = 0wx80), Z = (v=0w0)}} end
    | LDX x => let val v = value (regs, x) in regs # {X = v, P = (#P regs) # {N = (Word8.andb (v, 0wx80) = 0wx80), Z = (v=0w0)}} end
    | LDY x => regs
    | LSR x => regs
    | NOP x => regs
    | ORA x => regs
    | PHA x => regs
    | PHP x => regs
    | PLA x => regs
    | PLP x => regs
    | ROL x => regs
    | ROR x => regs
    | RTI x => regs
    | RTS x => (regs # {PC = read16 (0wx100 + w8ToW16 ((#S regs) + 0w1)), S = (#S regs) + 0w2})
    | SBC x => regs
    | SEC x => (regs # {P = (#P regs) # {C = true}})
    | SED x => regs
    | SEI x => regs
    | STA x => (write (address (regs, x), #A regs); regs)
    | STX x => (write (address (regs, x), #X regs); regs)
    | STY x => (write (address (regs, x), #Y regs); regs)
    | TAX x => regs
    | TAY x => regs
    | TSX x => regs
    | TXA x => regs
    | TXS x => regs
    | TYA x => regs

exception FailedDecode
fun fetchAndDecode (pc: word16): instruction * word16 =
    let
      fun imm _ = Immediate (read (pc + 0w1))
      fun zp  _ = ZeroPage (read (pc + 0w1))
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
        0wx00 => (BRK Implied, pc + 0w1)
      | 0wx01 => (ORA (Address (IndexedIndirect (read (pc + 0w1)))), pc + 0w2)
      | 0wx05 => (ORA (Address (ZeroPage (read (pc + 0w1)))), pc + 0w2)
      | 0wx06 => (ASL (Address (ZeroPage (read (pc + 0w1)))), pc + 0w2)
      | 0wx08 => (PHP Implied, pc + 0w1)
      | 0wx09 => (ORA (Immediate (read (pc + 0w1))), pc + 0w2)
      | 0wx0A => (ASL (Address Accumulator), pc + 0w1)
      | 0wx0D => (ORA (Address (Absolute (read16 (pc + 0w1)))), pc + 0w3)
      | 0wx0E => (ASL (Address (Absolute (read16 (pc + 0w1)))), pc + 0w3)
      | 0wx10 => (BPL (Relative (read (pc + 0w1))), pc + 0w2)
      | 0wx11 => (ORA (Address (IndirectIndexed (read (pc + 0w1)))), pc + 0w2)
      | 0wx15 => (ORA (Address (ZeroPageIndexedX (read (pc + 0w1)))), pc + 0w2)
      | 0wx16 => (ASL (Address (ZeroPageIndexedX (read (pc + 0w1)))), pc + 0w2)  
      | 0wx18 => (CLC Implied, pc + 0w1)
      | 0wx19 => (ORA (Address (AbsoluteIndexedY (read16 (pc + 0w1)))), pc + 0w3)  
      | 0wx1D => (ORA (Address (AbsoluteIndexedX (read16 (pc + 0w1)))), pc + 0w3)  
      | 0wx1E => (ASL (Address (AbsoluteIndexedX (read16 (pc + 0w1)))), pc + 0w3)
      | 0wx20 => (JSR (Absolute (read16 (pc + 0w1))), pc + 0w3)
      | 0wx21 => (AND (Address (IndexedIndirect (read (pc + 0w1)))), pc + 0w2)
      | 0wx24 => (BIT (Address (zp ())), pc + 0w2)
      | 0wx25 => (AND (Address (zp ())), pc + 0w2)   
      | 0wx26 => (ROL (zp ()), pc + 0w2)
      | 0wx28 => (PLP Implied, pc + 0w1)    
      | 0wx29 => (AND (imm ()), pc + 0w2)
      | 0wx2A => (ROL Accumulator, pc + 0w1)
      | 0wx2C => (BIT (Address (abs ())), pc + 0w3)
      | 0wx2D => (AND (Address (abs ())), pc + 0w3)  
      | 0wx2E => (ROL (abs ()), pc + 0w3)
      | 0wx30 => (BMI (rel ()), pc + 0w2) 
      | 0wx31 => (AND (Address (izy ())), pc + 0w2)
      | 0wx35 => (AND (Address (zpx ())), pc + 0w2)
      | 0wx36 => (ROL (zpx ()), pc + 0w2)
      | 0wx38 => (SEC Implied, pc + 0w1)   
      | 0wx39 => (AND (Address (aby ())), pc + 0w3)
      | 0wx3D => (AND (Address (abx ())), pc + 0w3)
      | 0wx3E => (ROL (abx ()), pc + 0w3)
      | 0wx40 => (RTI Implied, pc + 0w1)
      | 0wx41 => (EOR (Address (izx ())), pc + 0w2)
      | 0wx45 => (EOR (Address (zp ())), pc + 0w2)
      | 0wx46 => (LSR (zp ()), pc + 0w2)
      | 0wx48 => (PHA Implied, pc + 0w1)
      | 0wx49 => (EOR  (imm ()), pc + 0w2)
      | 0wx4A => (LSR Accumulator, pc + 0w1)
      | 0wx4C => (JMP (Absolute (read16 (pc + 0w1))), pc + 0w3)
      | 0wx4D => (EOR (Address (abs ())), pc + 0w3)
      | 0wx4E => (LSR (abs ()), pc + 0w3)
      | 0wx50 => (BVC (rel ()), pc + 0w2)
      | 0wx51 => (EOR (Address (izy ())), pc + 0w2) 
      | 0wx55 => (EOR (Address (zpx ())), pc + 0w2)  
      | 0wx56 => (LSR (zpx ()), pc + 0w2)
      | 0wx58 => (CLI Implied, pc + 0w1)
      | 0wx59 => (EOR (Address (aby ())), pc + 0w3)
      | 0wx5D => (EOR (Address (abx ())), pc + 0w3)  
      | 0wx5E => (LSR (abx ()), pc + 0w3)
      | 0wx60 => (RTS Implied, pc + 0w1)   
      | 0wx61 => (ADC (Address (izx ())), pc + 0w2)
      | 0wx65 => (ADC (Address (zp ())), pc + 0w2)
      | 0wx66 => (ROR (zp ()), pc + 0w2)
      | 0wx68 => (PLA Implied, pc + 0w1)
      | 0wx69 => (ADC (imm ()), pc + 0w2)
      | 0wx6A => (ROR Accumulator, pc + 0w1)
      | 0wx6C => (JMP (ind ()), pc + 0w3)
      | 0wx6D => (ADC (Address (abs ())), pc + 0w3)
      | 0wx6E => (ROR (abs ()), pc + 0w3)
      | 0wx70 => (BVS (rel ()), pc + 0w2)
      | 0wx71 => (ADC (Address (izy ())), pc + 0w2)
      | 0wx75 => (ADC (Address (zpx ())), pc + 0w2)
      | 0wx76 => (ROR (zpx ()), pc + 0w2)
      | 0wx78 => (SEI Implied, pc + 0w1)
      | 0wx79 => (ADC (Address (aby ())), pc + 0w3)
      | 0wx7D => (ADC (Address (abx ())), pc + 0w3)  
      | 0wx7E => (ROR (abx ()), pc + 0w3)
      | 0wx81 => (STA (izx ()), pc + 0w2)
      | 0wx84 => (STY (zp ()), pc + 0w2) 
      | 0wx85 => (STA (zp ()), pc + 0w2)   
      | 0wx86 => (STX (ZeroPage (read (pc + 0w1))), pc + 0w2)
      | 0wx88 => (DEY Implied, pc + 0w1)   
      | 0wx8A => (TXA Implied, pc + 0w1)  
      | 0wx8C => (STY (abs ()), pc + 0w3) 
      | 0wx8D => (STA (abs ()), pc + 0w3)
      | 0wx8E => (STX (abs ()), pc + 0w3) 
      | 0wx90 => (BCC (rel ()), pc + 0w2)  
      | 0wx91 => (STA (izy ()), pc + 0w2) 
      | 0wx94 => (STY (zpx ()), pc + 0w2)
      | 0wx95 => (STA (zpx ()), pc + 0w2) 
      | 0wx96 => (STX (zpy ()), pc + 0w2)
      | 0wx98 => (TYA Implied, pc + 0w1)
      | 0wx99 => (STA (aby ()), pc + 0w3)
      | 0wx9A => (TXS Implied, pc + 0w1)
      | 0wx9D => (STA (abx ()), pc + 0w3)
      | 0wxA0 => (LDY (imm ()), pc + 0w2)  
      | 0wxA1 => (LDA (Address (izx ())), pc + 0w2)
      | 0wxA2 => (LDX (Immediate (read (pc + 0w1))), pc + 0w2)
      | 0wxA4 => (LDY (Address (zp ())), pc + 0w2)
      | 0wxA5 => (LDA (Address (ZeroPage (read (pc + 0w1)))), pc + 0w2)
      | 0wxA6 => (LDX (Address (zp ())), pc + 0w2)
      | 0wxA8 => (TAY Implied, pc + 0w1)  
      | 0wxA9 => (LDA (Immediate (read (pc + 0w1))), pc + 0w2)
      | 0wxAA => (TAX Implied, pc + 0w1)
      | 0wxAC => (LDY (Address (abs ())), pc + 0w3)
      | 0wxAD => (LDA (Address (Absolute (read16 (pc + 0w1)))), pc + 0w3) 
      | 0wxAE => (LDX (Address (abs ())), pc + 0w3)
      | 0wxB0 => (BCS (rel ()), pc + 0w2) 
      | 0wxB1 => (LDA (Address (izy ())), pc + 0w2)
      | 0wxB4 => (LDY (Address (zpx ())), pc + 0w2)  
      | 0wxB5 => (LDA (Address (zpx ())), pc + 0w2)
      | 0wxB6 => (LDX (Address (zpy ())), pc + 0w2)  
      | 0wxB8 => (CLV Implied, pc + 0w1)
      | 0wxB9 => (LDA (Address (aby ())), pc + 0w3)  
      | 0wxBA => (TSX Implied, pc + 0w1)
      | 0wxBC => (LDY (Address (abx ())), pc + 0w3)  
      | 0wxBD => (LDA (Address (abx ())), pc + 0w3)
      | 0wxBE => (LDX (Address (aby ())), pc + 0w3)  
      | 0wxC0 => (CPY (imm ()), pc + 0w2)
      | 0wxC1 => (CMP (Address (izx ())), pc + 0w2)
      | 0wxC4 => (CPY (Address (zp ())), pc + 0w2)
      | 0wxC5 => (CMP (Address (zp ())), pc + 0w2)
      | 0wxC6 => (DEC (zp ()), pc + 0w2)
      | 0wxC8 => (INY Implied, pc + 0w1)
      | 0wxC9 => (CMP (imm ()), pc + 0w2)
      | 0wxCA => (DEX Implied, pc + 0w1)
      | 0wxCC => (CPY (Address (abs ())), pc + 0w3)
      | 0wxCD => (CMP (Address (abs ())), pc + 0w3)  
      | 0wxCE => (DEC (abs ()), pc + 0w3)
      | 0wxD0 => (BNE (rel ()), pc + 0w2)
      | 0wxD1 => (CMP (Address (izy ())), pc + 0w2)
      | 0wxD5 => (CMP (Address (zpx ())), pc + 0w2)  
      | 0wxD6 => (DEC (zpx ()), pc + 0w2)
      | 0wxD8 => (CLD Implied, pc + 0w1)
      | 0wxD9 => (CMP (Address (aby ())), pc + 0w3)
      | 0wxDD => (CMP (Address (abx ())), pc + 0w3)  
      | 0wxDE => (DEC (abx ()), pc + 0w3)
      | 0wxE0 => (CPX (imm ()), pc + 0w2)
      | 0wxE1 => (SBC (Address (izx ())), pc + 0w2)
      | 0wxE4 => (CPX (Address (zp ())), pc + 0w2) 
      | 0wxE5 => (SBC (Address (zp ())), pc + 0w2)
      | 0wxE6 => (INC (zp ()), pc + 0w2)   
      | 0wxE8 => (INX Implied, pc + 0w1)
      | 0wxE9 => (SBC (imm ()), pc + 0w2)  
      | 0wxEA => (NOP Implied, pc + 0w1)
      | 0wxEC => (CPX (Address (abs ())), pc + 0w3)
      | 0wxED => (SBC (Address (abs ())), pc + 0w3)
      | 0wxEE => (INC (abs ()), pc + 0w3)  
      | 0wxF0 => (BEQ (rel ()), pc + 0w2)
      | 0wxF1 => (SBC (Address (izy ())), pc + 0w2)  
      | 0wxF5 => (SBC (Address (zpx ())), pc + 0w2)
      | 0wxF6 => (INC (zpx ()), pc + 0w2)  
      | 0wxF8 => (SED Implied, pc + 0w1)
      | 0wxF9 => (SBC (Address (aby ())), pc + 0w3)  
      | 0wxFD => (SBC (Address (abx ())), pc + 0w3)
      | 0wxFE => (INC (abx ()), pc + 0w3)   
      | _     => raise FailedDecode
    end

fun p2Word p =
    let
      fun pow r 0 = 0w1
        | pow r n = r * pow r (n-1);
    in
        (if #N  p  then pow 0w2 7 else 0w0)
      + (if #V  p  then pow 0w2 6 else 0w0)
      + (if #B5 p  then pow 0w2 5 else 0w0)
      + (if #B4 p  then pow 0w2 4 else 0w0)
      + (if #D  p  then pow 0w2 3 else 0w0)
      + (if #I  p  then pow 0w2 2 else 0w0)
      + (if #Z  p  then pow 0w2 1 else 0w0)
      + (if #C  p  then pow 0w2 0 else 0w0)
    end

fun prtRegs regs =
    print ( Word16.toString (#PC regs) 
          ^ "\tA:" ^ Word8.toString (#A regs)
          ^ "/X:"  ^ Word8.toString (#X regs) 
          ^ "/Y:"  ^ Word8.toString (#Y regs) 
          ^ "/P:"  ^ Word8.toString (p2Word (#P regs))
          ^ "/SP:" ^ Word8.toString (#S regs) 
          ^ "\n")


fun cpu regs =
    let
      val (insn, pc2) = fetchAndDecode (#PC regs)
      val newRegs = exec (regs # {PC = pc2}, insn)
    in
      (print (Word8.toString (read (#PC regs)) ^ "/"); prtRegs regs; cpu newRegs)
    end

val sr  = {N=false, V=false, B5=true, B4=false, D=false, I=true, Z=false, C=false}
(* ;cpu {A=0w0, X=0w0, Y=0w0, PC=0wxC000, S=0wxFD, P=0wx24}; *)
;cpu {A=0w0, X=0w0, Y=0w0, PC=0wxC000, S=0wxFD, P=sr};
