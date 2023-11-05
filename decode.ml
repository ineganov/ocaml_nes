(* Batch-mode NES emulator  *)
(* MIT License              *)
(* Ilya Neganov, 2023       *)

type opcode = ADC (* add with carry                   *)
            | AND (* and (with accumulator)           *)
            | ASL (* arithmetic shift left            *)
            | BCC (* branch on carry clear            *)
            | BCS (* branch on carry set              *)
            | BEQ (* branch on equal (zero set)       *)
            | BIT (* bit test                         *)
            | BMI (* branch on minus (negative set)   *)
            | BNE (* branch on not equal (zero clear) *)
            | BPL (* branch on plus (negative clear)  *)
            | BRK (* break / interrupt                *)
            | BVC (* branch on overflow clear         *)
            | BVS (* branch on overflow set           *)
            | CLC (* clear carry                      *)
            | CLD (* clear decimal                    *)
            | CLI (* clear interrupt disable          *)
            | CLV (* clear overflow                   *)
            | CMP (* compare (with accumulator)       *)
            | CPX (* compare with X                   *)
            | CPY (* compare with Y                   *)
            | DEC (* decrement                        *)
            | DEX (* decrement X                      *)
            | DEY (* decrement Y                      *)
            | EOR (* exclusive or (with accumulator)  *)
            | INC (* increment                        *)
            | INX (* increment X                      *)
            | INY (* increment Y                      *)
            | JMP (* jump                             *)
            | JSR (* jump subroutine                  *)
            | LDA (* load accumulator                 *)
            | LDX (* load X                           *)
            | LDY (* load Y                           *)
            | LSR (* logical shift right              *)
            | NOP (* no operation                     *)
            | ORA (* or with accumulator              *)
            | PHA (* push accumulator                 *)
            | PHP (* push processor status (SR)       *)
            | PLA (* pull accumulator                 *)
            | PLP (* pull processor status (SR)       *)
            | ROL (* rotate left                      *)
            | ROR (* rotate right                     *)
            | RTI (* return from interrupt            *)
            | RTS (* return from subroutine           *)
            | SBC (* subtract with carry              *)
            | SEC (* set carry                        *)
            | SED (* set decimal                      *)
            | SEI (* set interrupt disable            *)
            | STA (* store accumulator                *)
            | STX (* store X                          *)
            | STY (* store Y                          *)
            | TAX (* transfer accumulator to X        *)
            | TAY (* transfer accumulator to Y        *)
            | TSX (* transfer stack pointer to X      *)
            | TXA (* transfer X to accumulator        *)
            | TXS (* transfer X to stack pointer      *)
            | TYA (* transfer Y to accumulator        *)
            | ILL (* Illegal                          *)

type addr_mode = Accum    (* OPC A       | operand is AC (implied single byte instruction)                                                                    *)
               | Abs      (* OPC $LLHH   | operand is address $HHLL *                                                                                         *)
               | Abs_X    (* OPC $LLHH,X | operand is address; effective address is address incremented by X with carry **                                    *)
               | Abs_Y    (* OPC $LLHH,Y | operand is address; effective address is address incremented by Y with carry **                                    *)
               | Immed    (* OPC #$BB    | operand is byte                                                                                                    *)
               | Implied  (* OPC         | operand implied                                                                                                    *)
               | Ind      (* OPC ($LLHH) | operand is address; effective address is contents of word at address: C.w($HHLL)                                   *)
               | X_Ind    (* OPC ($LL,X) | operand is zeropage address; effective address is word in (LL + X, LL + X + 1), inc. without carry: C.w($00LL + X) *)
               | Ind_Y    (* OPC ($LL),Y | operand is zeropage address; effective address is word in (LL, LL + 1) incremented by Y with carry: C.w($00LL) + Y *)
               | Rel      (* OPC $BB     | branch target is PC + signed offset BB ***                                                                         *)
               | Zpg      (* OPC $LL     | operand is zeropage address (hi-byte is zero, address = $00LL)                                                     *)
               | Zpg_X    (* $LL,X       | operand is zeropage address; effective address is address incremented by X without carry **                        *)
               | Zpg_Y    (* OPC $LL,Y   | operand is zeropage address; effective address is address incremented by Y without carry **                        *)

type inst_size = SZ_1 | SZ_2 | SZ_3

type instn = Instr of opcode * addr_mode * inst_size * int


let decode i = match i with 
    (* === ADC === *)

    (* Add Memory to Accumulator with Carry *)
    (* A + M + C -> A, C *)
    (* NZCIDV *)
    (* +++--+ *)

      0x69 -> Instr (ADC, Immed , SZ_2, 2)
    | 0x65 -> Instr (ADC, Zpg   , SZ_2, 3)
    | 0x75 -> Instr (ADC, Zpg_X , SZ_2, 4)
    | 0x6D -> Instr (ADC, Abs   , SZ_3, 4)
    | 0x7D -> Instr (ADC, Abs_X , SZ_3, 4)
    | 0x79 -> Instr (ADC, Abs_Y , SZ_3, 4)
    | 0x61 -> Instr (ADC, X_Ind , SZ_2, 6)
    | 0x71 -> Instr (ADC, Ind_Y , SZ_2, 5)


    (* === AND === *)

    (* AND Memory with Accumulator *)
    (* A AND M -> A *)
    (* NZCIDV *)
    (* ++---- *)

    | 0x29 -> Instr ( AND, Immed, SZ_2, 2 )
    | 0x25 -> Instr ( AND, Zpg,   SZ_2, 3 )
    | 0x35 -> Instr ( AND, Zpg_X, SZ_2, 4 )
    | 0x2D -> Instr ( AND, Abs,   SZ_3, 4 )
    | 0x3D -> Instr ( AND, Abs_X, SZ_3, 4 )
    | 0x39 -> Instr ( AND, Abs_Y, SZ_3, 4 )
    | 0x21 -> Instr ( AND, X_Ind, SZ_2, 6 )
    | 0x31 -> Instr ( AND, Ind_Y, SZ_2, 5 )


    (* === ASL === *)

    (* Shift Left One Bit (Memory or Accumulator) *)
    (* C <- [76543210] <- 0 *)
    (* NZCIDV *)
    (* +++--- *)

    | 0x0A -> Instr ( ASL, Accum, SZ_1, 2 )
    | 0x06 -> Instr ( ASL, Zpg,   SZ_2, 5 )
    | 0x16 -> Instr ( ASL, Zpg_X, SZ_2, 6 )
    | 0x0E -> Instr ( ASL, Abs,   SZ_3, 6 )
    | 0x1E -> Instr ( ASL, Abs_X, SZ_3, 7 )
    

    (* === BCC === *)

    (* Branch on Carry Clear *)
    (* branch on C = 0 *)
    (* NZCIDV *)
    (* ------ *)

    | 0x90 -> Instr ( BCC, Rel, SZ_2, 2 )
    

    (* === BCS === *)

    (* Branch on Carry Set *)
    (* branch on C = 1 *)
    (* NZCIDV *)
    (* ------ *)

    | 0xB0 -> Instr ( BCS, Rel, SZ_2, 2 )
    

    (* === BEQ === *)

    (* Branch on Result Zero *)
    (* branch on Z = 1 *)
    (* NZCIDV *)
    (* ------ *)

    | 0xF0 -> Instr ( BEQ, Rel, SZ_2, 2 )
    

    (* === BIT === *)

    (* Test Bits in Memory with Accumulator *)
    (* bits 7 and 6 of operand are transfered to bit 7 and 6 of SR (N,V); *)
    (* the zero-flag is set to the result of operand AND Accum. *)
    (* A AND M, M7 -> N, M6 -> V *)
    (* NZCIDV *)
    (* M7+---M6 *)

    | 0x24 -> Instr ( BIT, Zpg, SZ_2, 3 )
    | 0x2C -> Instr ( BIT, Abs, SZ_3, 4 )
    

    (* === BMI === *)

    (* Branch on Result Minus *)
    (* branch on N = 1 *)
    (* NZCIDV *)
    (* ------ *)

    | 0x30 -> Instr ( BMI, Rel, SZ_2, 2 )
    

    (* === BNE === *)

    (* Branch on Result not Zero *)
    (* branch on Z = 0 *)
    (* NZCIDV *)
    (* ------ *)

    | 0xD0 -> Instr ( BNE, Rel, SZ_2, 2 )
    

    (* === BPL === *)

    (* Branch on Result Plus *)
    (* branch on N = 0 *)
    (* NZCIDV *)
    (* ------ *)

    | 0x10 -> Instr ( BPL, Rel, SZ_2, 2 )
    

    (* === BRK === *)

    (* Force Break *)

    (* BRK initiates a software interrupt similar to a hardware *)
    (* interrupt (IRQ). Theaddress pushed to the stack is *)
    (* PC+2, providing an extra byte of spacing for a break mark *)
    (* (identifying a reason for the break.) *)
    (* The status register will be pushed to the stack with the break *)
    (* flag set to 1. However, when retrieved during RTI or by a PLP *)
    (* instruction, the break flag will be ignored. *)
    (* The interrupt disable flag is not set automatically. *)
    (* interrupt, *)
    (* push PC+2, push SR *)
    (* NZCIDV *)
    (* ---1-- *)
    
    | 0x00 -> Instr ( BRK, Implied, SZ_2, 7 )


    (* === BVC === *)

    (* Branch on Overflow Clear *)
    (* branch on V = 0 *)
    (* NZCIDV *)
    (* ------ *)

    | 0x50 -> Instr ( BVC, Rel, SZ_2, 2 )
    

    (* === BVS === *)

    (* Branch on Overflow Set *)
    (* branch on V = 1 *)
    (* NZCIDV *)
    (* ------ *)

    | 0x70 -> Instr ( BVS, Rel, SZ_2, 2 )
    

    (* === CLC === *)

    (* Clear Carry Flag *)
    (* 0 -> C *)
    (* NZCIDV *)
    (* --0--- *)

    | 0x18 -> Instr ( CLC, Implied, SZ_1, 2 )


    (* === CLD === *)

    (* Clear Decimal Mode *)
    (* 0 -> D *)
    (* NZCIDV *)
    (* ----0- *)

    | 0xD8 -> Instr ( CLD, Implied, SZ_1, 2 )
    

    (* === CLI === *)

    (* Clear Interrupt Disable Bit *)
    (* 0 -> I *)
    (* NZCIDV *)
    (* ---0-- *)

    | 0x58 -> Instr ( CLI, Implied, SZ_1, 2 )
    

    (* === CLV === *)

    (* Clear Overflow Flag *)
    (* 0 -> V *)
    (* NZCIDV *)
    (* -----0 *)

    | 0xB8 -> Instr ( CLV, Implied, SZ_1, 2 )
    

    (* === CMP === *)

    (* Compare Memory with Accumulator *)
    (* A - M *)
    (* NZCIDV *)
    (* +++--- *)

    | 0xC9 -> Instr ( CMP, Immed, SZ_2, 2 )
    | 0xC5 -> Instr ( CMP, Zpg,   SZ_2, 3 )
    | 0xD5 -> Instr ( CMP, Zpg_X, SZ_2, 4 )
    | 0xCD -> Instr ( CMP, Abs,   SZ_3, 4 )
    | 0xDD -> Instr ( CMP, Abs_X, SZ_3, 4 )
    | 0xD9 -> Instr ( CMP, Abs_Y, SZ_3, 4 )
    | 0xC1 -> Instr ( CMP, X_Ind, SZ_2, 6 )
    | 0xD1 -> Instr ( CMP, Ind_Y, SZ_2, 5 )


    (* === CPX === *)

    (* Compare Memory and Index X *)
    (* X - M *)
    (* NZCIDV *)
    (* +++--- *)

    | 0xE0 -> Instr ( CPX, Immed, SZ_2, 2 )
    | 0xE4 -> Instr ( CPX, Zpg,   SZ_2, 3 )
    | 0xEC -> Instr ( CPX, Abs,   SZ_3, 4 )
    

    (* === CPY === *)

    (* Compare Memory and Index Y *)
    (* Y - M *)
    (* NZCIDV *)
    (* +++--- *)

    | 0xC0 -> Instr ( CPY, Immed, SZ_2, 2 )
    | 0xC4 -> Instr ( CPY, Zpg,   SZ_2, 3 )
    | 0xCC -> Instr ( CPY, Abs,   SZ_3, 4 )
    

    (* === DEC === *)

    (* Decrement Memory by One *)
    (* M - 1 -> M *)
    (* NZCIDV *)
    (* ++---- *)

    | 0xC6 -> Instr ( DEC, Zpg,   SZ_2, 5 )
    | 0xD6 -> Instr ( DEC, Zpg_X, SZ_2, 6 )
    | 0xCE -> Instr ( DEC, Abs,   SZ_3, 6 )
    | 0xDE -> Instr ( DEC, Abs_X, SZ_3, 7 )
    

    (* === DEX === *)

    (* Decrement Index X by One *)
    (* X - 1 -> X *)
    (* NZCIDV *)
    (* ++---- *)

    | 0xCA -> Instr ( DEX, Implied, SZ_1, 2 )
    

    (* === DEY === *)

    (* Decrement Index Y by One *)
    (* Y - 1 -> Y *)
    (* NZCIDV *)
    (* ++---- *)

    | 0x88 -> Instr ( DEY, Implied, SZ_1, 2 )


    (* === EOR === *)

    (* Exclusive-OR Memory with Accumulator *)
    (* A EOR M -> A *)
    (* NZCIDV *)
    (* ++---- *)

    | 0x49 -> Instr ( EOR, Immed, SZ_2, 2 )
    | 0x45 -> Instr ( EOR, Zpg,   SZ_2, 3 )
    | 0x55 -> Instr ( EOR, Zpg_X, SZ_2, 4 )
    | 0x4D -> Instr ( EOR, Abs,   SZ_3, 4 )
    | 0x5D -> Instr ( EOR, Abs_X, SZ_3, 4 )
    | 0x59 -> Instr ( EOR, Abs_Y, SZ_3, 4 )
    | 0x41 -> Instr ( EOR, X_Ind, SZ_2, 6 )
    | 0x51 -> Instr ( EOR, Ind_Y, SZ_2, 5 )


    (* === INC === *)

    (* Increment Memory by One *)
    (* M + 1 -> M *)
    (* NZCIDV *)
    (* ++---- *)

    | 0xE6 -> Instr ( INC, Zpg,   SZ_2, 5 )
    | 0xF6 -> Instr ( INC, Zpg_X, SZ_2, 6 )
    | 0xEE -> Instr ( INC, Abs,   SZ_3, 6 )
    | 0xFE -> Instr ( INC, Abs_X, SZ_3, 7 )


    (* === INX === *)

    (* Increment Index X by One *)
    (* X + 1 -> X *)
    (* NZCIDV *)
    (* ++---- *)

    | 0xE8 -> Instr ( INX, Implied, SZ_1, 2 )


    (* === INY === *)

    (* Increment Index Y by One *)
    (* Y + 1 -> Y *)
    (* NZCIDV *)
    (* ++---- *)

    | 0xC8 -> Instr ( INY, Implied, SZ_1, 2 )


    (* === JMP === *)

    (* Jump to New Location *)
    (* (PC+1) -> PCL *)
    (* (PC+2) -> PCH *)
    (* NZCIDV *)
    (* ------ *)

    | 0x4C -> Instr ( JMP, Abs, SZ_3, 3 )
    | 0x6C -> Instr ( JMP, Ind, SZ_3, 5 )
    

    (* === JSR === *)

    (* Jump to New Location Saving Return Address *)

    (* push (PC+2), *)
    (* (PC+1) -> PCL *)
    (* (PC+2) -> PCH *)
    (* NZCIDV *)
    (* ------ *)

    | 0x20 -> Instr ( JSR, Abs, SZ_3, 6 )
    

    (* === LDA === *)

    (* Load Accumulator with Memory *)
    (* M -> A *)
    (* NZCIDV *)
    (* ++---- *)

    | 0xA9 -> Instr ( LDA, Immed, SZ_2, 2 )
    | 0xA5 -> Instr ( LDA, Zpg,   SZ_2, 3 )
    | 0xB5 -> Instr ( LDA, Zpg_X, SZ_2, 4 )
    | 0xAD -> Instr ( LDA, Abs,   SZ_3, 4 )
    | 0xBD -> Instr ( LDA, Abs_X, SZ_3, 4 )
    | 0xB9 -> Instr ( LDA, Abs_Y, SZ_3, 4 )
    | 0xA1 -> Instr ( LDA, X_Ind, SZ_2, 6 )
    | 0xB1 -> Instr ( LDA, Ind_Y, SZ_2, 5 )
    

    (* === LDX === *)

    (* Load Index X with Memory *)
    (* M -> X *)
    (* NZCIDV *)
    (* ++---- *)

    | 0xA2 -> Instr ( LDX, Immed, SZ_2, 2 )
    | 0xA6 -> Instr ( LDX, Zpg,   SZ_2, 3 )
    | 0xB6 -> Instr ( LDX, Zpg_Y, SZ_2, 4 )
    | 0xAE -> Instr ( LDX, Abs,   SZ_3, 4 )
    | 0xBE -> Instr ( LDX, Abs_Y, SZ_3, 4 )
    

    (* === LDY === *)

    (* Load Index Y with Memory *)
    (* M -> Y *)
    (* NZCIDV *)
    (* ++---- *)

    | 0xA0 -> Instr ( LDY, Immed, SZ_2, 2 )
    | 0xA4 -> Instr ( LDY, Zpg,   SZ_2, 3 )
    | 0xB4 -> Instr ( LDY, Zpg_X, SZ_2, 4 )
    | 0xAC -> Instr ( LDY, Abs,   SZ_3, 4 )
    | 0xBC -> Instr ( LDY, Abs_X, SZ_3, 4 )
    

    (* === LSR === *)

    (* Shift One Bit Right (Memory or Accumulator) *)
    (* 0 -> [76543210] -> C *)
    (* NZCIDV *)
    (* 0++--- *)

    | 0x4A -> Instr ( LSR, Accum, SZ_1, 2 )
    | 0x46 -> Instr ( LSR, Zpg,   SZ_2, 5 )
    | 0x56 -> Instr ( LSR, Zpg_X, SZ_2, 6 )
    | 0x4E -> Instr ( LSR, Abs,   SZ_3, 6 )
    | 0x5E -> Instr ( LSR, Abs_X, SZ_3, 7 )
    

    (* === NOP === *)

    (* No Operation *)
    (* === *)
    (* NZCIDV *)
    (* ------ *)

    | 0xEA -> Instr ( NOP, Implied, SZ_1, 2 )
    

    (* === ORA === *)

    (* OR Memory with Accumulator *)
    (* A OR M -> A *)
    (* NZCIDV *)
    (* ++---- *)

    | 0x09 -> Instr ( ORA, Immed, SZ_2, 2 )
    | 0x05 -> Instr ( ORA, Zpg,   SZ_2, 3 )
    | 0x15 -> Instr ( ORA, Zpg_X, SZ_2, 4 )
    | 0x0D -> Instr ( ORA, Abs,   SZ_3, 4 )
    | 0x1D -> Instr ( ORA, Abs_X, SZ_3, 4 )
    | 0x19 -> Instr ( ORA, Abs_Y, SZ_3, 4 )
    | 0x01 -> Instr ( ORA, X_Ind, SZ_2, 6 )
    | 0x11 -> Instr ( ORA, Ind_Y, SZ_2, 5 )
    

    (* === PHA === *)

    (* Push Accumulator on Stack *)
    (* push A *)
    (* NZCIDV *)
    (* ------ *)

    | 0x48 -> Instr ( PHA, Implied, SZ_1, 3 )
    

    (* === PHP === *)

    (* Push Processor Status on Stack *)

    (* The status register will be pushed with the break *)
    (* flag and bit 5 set to 1. *)
    (* push SR *)
    (* NZCIDV *)
    (* ------ *)

    | 0x08 -> Instr ( PHP, Implied, SZ_1, 3 )


    (* === PLA === *)

    (* Pull Accumulator from Stack *)
    (* pull A *)
    (* NZCIDV *)
    (* ++---- *)

    | 0x68 -> Instr ( PLA, Implied, SZ_1, 4 )


    (* === PLP === *)

    (* Pull Processor Status from Stack *)

    (* The status register will be pulled with the break *)
    (* flag and bit 5 ignored. *)
    (* pull SR *)
    (* NZCIDV *)
    (* from stack *)

    | 0x28 -> Instr ( PLP, Implied, SZ_1, 4 )


    (* === ROL === *)

    (* Rotate One Bit Left (Memory or Accumulator) *)
    (* C <- [76543210] <- C *)
    (* NZCIDV *)
    (* +++--- *)

    | 0x2A -> Instr ( ROL, Accum, SZ_1, 2 )
    | 0x26 -> Instr ( ROL, Zpg,   SZ_2, 5 )
    | 0x36 -> Instr ( ROL, Zpg_X, SZ_2, 6 )
    | 0x2E -> Instr ( ROL, Abs,   SZ_3, 6 )
    | 0x3E -> Instr ( ROL, Abs_X, SZ_3, 7 )
    

    (* === ROR === *)

    (* Rotate One Bit Right (Memory or Accumulator) *)
    (* C -> [76543210] -> C *)
    (* NZCIDV *)
    (* +++--- *)

    | 0x6A -> Instr ( ROR, Accum, SZ_1, 2 )
    | 0x66 -> Instr ( ROR, Zpg,   SZ_2, 5 )
    | 0x76 -> Instr ( ROR, Zpg_X, SZ_2, 6 )
    | 0x6E -> Instr ( ROR, Abs,   SZ_3, 6 )
    | 0x7E -> Instr ( ROR, Abs_X, SZ_3, 7 )
    

    (* === RTI === *)

    (* Return from Interrupt *)

    (* The status register is pulled with the break flag *)
    (* and bit 5 ignored. Then PC is pulled from the stack. *)
    (* pull SR, pull PC *)
    (* NZCIDV *)
    (* from stack *)

    | 0x40 -> Instr ( RTI, Implied, SZ_1, 6 )
    

    (* === RTS === *)

    (* Return from Subroutine *)
    (* pull PC, PC+1 -> PC *)
    (* NZCIDV *)
    (* ------ *)

    | 0x60 -> Instr ( RTS, Implied, SZ_1, 6 )
    
    
    (* === SBC === *)

    (* Subtract Memory from Accumulator with Borrow *)
    (* A - M - C -> A *)
    (* NZCIDV *)
    (* +++--+ *)

    | 0xE9 -> Instr ( SBC, Immed, SZ_2, 2 )
    | 0xE5 -> Instr ( SBC, Zpg,   SZ_2, 3 )
    | 0xF5 -> Instr ( SBC, Zpg_X, SZ_2, 4 )
    | 0xED -> Instr ( SBC, Abs,   SZ_3, 4 )
    | 0xFD -> Instr ( SBC, Abs_X, SZ_3, 4 )
    | 0xF9 -> Instr ( SBC, Abs_Y, SZ_3, 4 )
    | 0xE1 -> Instr ( SBC, X_Ind, SZ_2, 6 )
    | 0xF1 -> Instr ( SBC, Ind_Y, SZ_2, 5 )
    

    (* === SEC === *)

    (* Set Carry Flag *)
    (* 1 -> C *)
    (* NZCIDV *)
    (* --1--- *)

    | 0x38 -> Instr ( SEC, Implied, SZ_1, 2 )
    

    (* === SED === *)

    (* Set Decimal Flag *)
    (* 1 -> D *)
    (* NZCIDV *)
    (* ----1- *)


    | 0xF8 -> Instr ( SED, Implied, SZ_1, 2 )
    
    (* === SEI === *)

    (* Set Interrupt Disable Status *)
    (* 1 -> I *)
    (* NZCIDV *)
    (* ---1-- *)


    | 0x78 -> Instr ( SEI, Implied, SZ_1, 2 )


    (* === STA === *)

    (* Store Accumulator in Memory *)
    (* A -> M *)
    (* NZCIDV *)
    (* ------ *)

    | 0x85 -> Instr ( STA, Zpg,   SZ_2, 3 )
    | 0x95 -> Instr ( STA, Zpg_X, SZ_2, 4 )
    | 0x8D -> Instr ( STA, Abs,   SZ_3, 4 )
    | 0x9D -> Instr ( STA, Abs_X, SZ_3, 5 )
    | 0x99 -> Instr ( STA, Abs_Y, SZ_3, 5 )
    | 0x81 -> Instr ( STA, X_Ind, SZ_2, 6 )
    | 0x91 -> Instr ( STA, Ind_Y, SZ_2, 6 )


    (* === STX === *)

    (* Store Index X in Memory *)
    (* X -> M *)
    (* NZCIDV *)
    (* ------ *)

    | 0x86 -> Instr ( STX, Zpg,   SZ_2, 3 )
    | 0x96 -> Instr ( STX, Zpg_Y, SZ_2, 4 )
    | 0x8E -> Instr ( STX, Abs,   SZ_3, 4 )


    (* === STY === *)

    (* Sore Index Y in Memory *)
    (* Y -> M *)
    (* NZCIDV *)
    (* ------ *)

    | 0x84 -> Instr ( STY, Zpg,   SZ_2, 3 )
    | 0x94 -> Instr ( STY, Zpg_X, SZ_2, 4 )
    | 0x8C -> Instr ( STY, Abs,   SZ_3, 4 )


    (* === TAX === *)

    (* Transfer Accumulator to Index X *)
    (* A -> X *)
    (* NZCIDV *)
    (* ++---- *)

    | 0xAA -> Instr ( TAX, Implied, SZ_1, 2 )


    (* === TAY === *)

    (* Transfer Accumulator to Index Y *)
    (* A -> Y *)
    (* NZCIDV *)
    (* ++---- *)

    | 0xA8 -> Instr ( TAY, Implied, SZ_1, 2 )
    

    (* === TSX === *)

    (* Transfer Stack Pointer to Index X *)
    (* SP -> X *)
    (* NZCIDV *)
    (* ++---- *)

    | 0xBA -> Instr ( TSX, Implied, SZ_1, 2 )
    

    (* === TXA === *)

    (* Transfer Index X to Accumulator *)
    (* X -> A *)
    (* NZCIDV *)
    (* ++---- *)

    | 0x8A -> Instr ( TXA, Implied, SZ_1, 2 )
    

    (* === TXS === *)

    (* Transfer Index X to Stack Register *)
    (* X -> SP *)
    (* NZCIDV *)
    (* ------ *)

    | 0x9A -> Instr ( TXS, Implied, SZ_1, 2 )
    

    (* === TYA === *)

    (* Transfer Index Y to Accumulator *)
    (* Y -> A *)
    (* NZCIDV *)
    (* ++---- *)

    | 0x98 -> Instr ( TYA, Implied, SZ_1, 2 )

    | _    -> Instr (ILL , Implied , SZ_1 , 2)


let show_opcode = function
      ADC -> "ADC"
    | AND -> "AND"
    | ASL -> "ASL"
    | BCC -> "BCC"
    | BCS -> "BCS"
    | BEQ -> "BEQ"
    | BIT -> "BIT"
    | BMI -> "BMI"
    | BNE -> "BNE"
    | BPL -> "BPL"
    | BRK -> "BRK"
    | BVC -> "BVC"
    | BVS -> "BVS"
    | CLC -> "CLC"
    | CLD -> "CLD"
    | CLI -> "CLI"
    | CLV -> "CLV"
    | CMP -> "CMP"
    | CPX -> "CPX"
    | CPY -> "CPY"
    | DEC -> "DEC"
    | DEX -> "DEX"
    | DEY -> "DEY"
    | EOR -> "EOR"
    | INC -> "INC"
    | INX -> "INX"
    | INY -> "INY"
    | JMP -> "JMP"
    | JSR -> "JSR"
    | LDA -> "LDA"
    | LDX -> "LDX"
    | LDY -> "LDY"
    | LSR -> "LSR"
    | NOP -> "NOP"
    | ORA -> "ORA"
    | PHA -> "PHA"
    | PHP -> "PHP"
    | PLA -> "PLA"
    | PLP -> "PLP"
    | ROL -> "ROL"
    | ROR -> "ROR"
    | RTI -> "RTI"
    | RTS -> "RTS"
    | SBC -> "SBC"
    | SEC -> "SEC"
    | SED -> "SED"
    | SEI -> "SEI"
    | STA -> "STA"
    | STX -> "STX"
    | STY -> "STY"
    | TAX -> "TAX"
    | TAY -> "TAY"
    | TSX -> "TSX"
    | TXA -> "TXA"
    | TXS -> "TXS"
    | TYA -> "TYA"
    | ILL -> "ILL"

let show_instn i = match i with
    Instr (op, Accum   , _ , _) -> (show_opcode op) ^ " Accum"
  | Instr (op, Abs     , _ , _) -> (show_opcode op) ^ " $LLHH"
  | Instr (op, Abs_X   , _ , _) -> (show_opcode op) ^ " $LLHH,X"
  | Instr (op, Abs_Y   , _ , _) -> (show_opcode op) ^ " $LLHH,Y"
  | Instr (op, Immed   , _ , _) -> (show_opcode op) ^ " #$BB"
  | Instr (op, Implied , _ , _) -> (show_opcode op)
  | Instr (op, Ind     , _ , _) -> (show_opcode op) ^ " ($LLHH)"
  | Instr (op, X_Ind   , _ , _) -> (show_opcode op) ^ " ($LL,X)"
  | Instr (op, Ind_Y   , _ , _) -> (show_opcode op) ^ " ($LL),Y"
  | Instr (op, Rel     , _ , _) -> (show_opcode op) ^ " $BB"
  | Instr (op, Zpg     , _ , _) -> (show_opcode op) ^ " $LL"
  | Instr (op, Zpg_X   , _ , _) -> (show_opcode op) ^ " $LL,X"
  | Instr (op, Zpg_Y   , _ , _) -> (show_opcode op) ^ " $LL,Y"

let sprintf = Printf.sprintf

let show_full_instn i pc p = match i with
  Instr (op, Accum   , _ , _) -> sprintf "%04x: %s"            pc (show_opcode op)
| Instr (op, Abs     , _ , _) -> sprintf "%04x: %s  0x%04X"     pc (show_opcode op) p
| Instr (op, Abs_X   , _ , _) -> sprintf "%04x: %s  0x%04X,X"   pc (show_opcode op) p 
| Instr (op, Abs_Y   , _ , _) -> sprintf "%04x: %s  0x%04X,Y"   pc (show_opcode op) p
| Instr (op, Immed   , _ , _) -> sprintf "%04x: %s  #0x%02X"    pc (show_opcode op) p
| Instr (op, Implied , _ , _) -> sprintf "%04x: %s"            pc (show_opcode op)
| Instr (op, Ind     , _ , _) -> sprintf "%04x: %s  (0x%04X)"   pc (show_opcode op) p
| Instr (op, X_Ind   , _ , _) -> sprintf "%04x: %s  (0x%02X,X)" pc (show_opcode op) p
| Instr (op, Ind_Y   , _ , _) -> sprintf "%04x: %s  (0x%02X),Y" pc (show_opcode op) p
| Instr (op, Zpg     , _ , _) -> sprintf "%04x: %s   0x%02X"    pc (show_opcode op) p
| Instr (op, Zpg_X   , _ , _) -> sprintf "%04x: %s   0x%02X,X"  pc (show_opcode op) p
| Instr (op, Zpg_Y   , _ , _) -> sprintf "%04x: %s   0x%02X,Y"  pc (show_opcode op) p
| Instr (op, Rel     , _ , _) -> if (0x80 land p) = 0 then sprintf "%04x: %s   0x%02X" pc (show_opcode op) p
                                                      else sprintf "%04x: %s  -0x%02X" pc (show_opcode op) (1 + (0xFF lxor p))


let isize_incr = function SZ_1 -> 1
                        | SZ_2 -> 2
                        | SZ_3 -> 3

let is_not_store op = op <> STA && op <> STX && op <> STY
