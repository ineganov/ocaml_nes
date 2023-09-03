open Decode

let printf     = Printf.printf
let fprintf    = Printf.fprintf
let cc buf idx = Char.code (Bytes.get buf idx)


exception Not_A_Nes_Fmt
exception Not_A_Nes_Palette
exception Break_Instn
exception Illegal_Instn
exception ShouldNeverHappen

let chk_hdr bbuf = (Bytes.sub bbuf 0 3) = (Bytes.of_string "NES") && (cc bbuf 3 = 0x1a)

let pp_bytes prefix buf list = printf "%s " prefix;
                               List.iter (printf "%02x ") (List.map (fun x -> cc buf x) list) ;
                               printf "\n"

let fetch_segments (path : string) : (bytes * bytes) = 
               let fd      = open_in path                in
               let flen    = in_channel_length fd        in
               let bbuf    = Bytes.create flen           in
               let ()      = really_input fd bbuf 0 flen in (* reading the exact number of bytes does not throw EOFile *)
               let ()      = if not (chk_hdr bbuf) then raise Not_A_Nes_Fmt  in
               let prg_len = (cc bbuf 4) in
               let chr_len = (cc bbuf 5) in
               let flgs_6  = (cc bbuf 6) in
               let ()      = printf "Num segments PRG: %d CHR: %d FLG6: %02x\n"  prg_len chr_len flgs_6 in
                      ( Bytes.sub bbuf 16 (prg_len * 16384) ,
                        Bytes.sub bbuf (16 + prg_len * 16384) (chr_len * 8192) )

let load_palette path = let fd    = open_in path                in
                        let flen  = in_channel_length fd        in
                        let bbuf  = Bytes.create flen           in
                        let ()    = really_input fd bbuf 0 flen in
                        let arr   = Array.make 64 (0,0,0)       in
                          if flen <> 192 then raise Not_A_Nes_Palette ;
                          for idx = 0 to 63 do
                            arr.(idx) <- (Bytes.get_uint8 bbuf (idx * 3 + 0),
                                          Bytes.get_uint8 bbuf (idx * 3 + 1),
                                          Bytes.get_uint8 bbuf (idx * 3 + 2))
                          done ;
                          arr


type machine = {  mutable pc     : int ;
                  mutable sp     : int ;
                  mutable acc    : int ;
                  mutable x      : int ;
                  mutable y      : int ;
                  mutable flg_n  : int ;
                  mutable flg_v  : int ;
                  mutable flg_b  : int ;
                  mutable flg_d  : int ;
                  mutable flg_i  : int ;
                  mutable flg_z  : int ;
                  mutable flg_c  : int ;
                  mutable ram    : bytes ;
                  mutable rom    : bytes ;
                  mutable cycles : int ;

                  mutable chr_rom     : bytes ;
                  mutable ppu_ctrl    : int ;
                  mutable ppu_wr_addr : int ;
                  mutable ppu_ram     : bytes ;
                  mutable plt_idx     : int array ;
                  mutable plt         : (int * int * int) array
                }


let mk_machine prg_mem chr_mem plt_mem = let reset_vec = Bytes.get_uint16_le prg_mem (16384 - 4) in
                            { pc     = reset_vec ;
                              sp     = 0xFD ;
                              acc    = 0 ;
                              x      = 0 ;
                              y      = 0 ;
                              flg_n  = 0 ;
                              flg_v  = 0 ;
                              flg_b  = 0 ;
                              flg_d  = 0 ;
                              flg_i  = 0 ;
                              flg_z  = 0 ;
                              flg_c  = 0 ;
                              cycles = 0 ;
                              ram = Bytes.make 2048 '\000' ;
                              rom = prg_mem ;
                              chr_rom = chr_mem ;
                              ppu_ctrl = 0;
                              ppu_wr_addr = 0 ;
                              ppu_ram = Bytes.make 2048 '\000' ;
                              plt_idx = Array.make 32 0 ;
                              plt = plt_mem}

let status_byte cpu = (cpu.flg_n lsl 7)
                  lor (cpu.flg_v lsl 6)
                  lor (        1 lsl 5)
                  lor (cpu.flg_b lsl 4)
                  lor (cpu.flg_d lsl 3)
                  lor (cpu.flg_i lsl 2)
                  lor (cpu.flg_z lsl 1)
                  lor (cpu.flg_c lsl 0)

let set_flags cpu flgs = ( (* ignires break in both rti and plp *)
   cpu.flg_n <- (0x80 land flgs) lsr 7;
   cpu.flg_v <- (0x40 land flgs) lsr 6;
(* cpu.flg_b <- (0x10 land flgs) lsr 4; *)
   cpu.flg_d <- (0x08 land flgs) lsr 3;
   cpu.flg_i <- (0x04 land flgs) lsr 2;
   cpu.flg_z <- (0x02 land flgs) lsr 1;
   cpu.flg_c <- (0x01 land flgs) lsr 0 )


let show_flags cpu = let cap_str vs = List.map (fun (x,y) -> if x > 0 then String.uppercase_ascii y else y) vs in
      String.concat "" (cap_str [ (cpu.flg_n, "n") ;
                                  (cpu.flg_v, "v") ;
                                  (cpu.flg_b, "b") ;
                                  (cpu.flg_d, "d") ;
                                  (cpu.flg_i, "i") ;
                                  (cpu.flg_z, "z") ;
                                  (cpu.flg_c, "c") ; ])

let show_cpu cpu = Printf.sprintf "ACC:%02X X:%02X Y:%02X SP:%02X -- %s" cpu.acc cpu.x cpu.y cpu.sp (show_flags cpu)

let ppu_ram_wr_hmap cpu v = match cpu.ppu_wr_addr with
                                   addr when addr >= 0x2000 && addr < 0x2400 -> Bytes.set_uint8 cpu.ppu_ram (addr land 0x3FF) v
                                 | addr when addr >= 0x2800 && addr < 0x2C00 -> Bytes.set_uint8 cpu.ppu_ram ((addr land 0x3FF) lor 0x400) v
                                 | addr when addr >= 0x3f00 && addr < 0x3f20 -> cpu.plt_idx.(addr land 0x1F) <- v
                                 | other -> printf "PPU WR other: %04x\n" other

let ppu_draw_vram cpu file = let fd = open_out file in
                                fprintf fd "P3 256 512 255\n" ;
                                for pix_y = 0 to 511 do  (* up to 239 for single screen *)
                                   for tile_x = 0 to 31 do
                                      let tile_y       = pix_y lsr 3                                      in
                                      let tile_y_mod   = tile_y land 0x1F                                 in
                                      let nm_tbl_byte  = cc cpu.ppu_ram (32 * tile_y + tile_x)            in
                                      let fine_y       = pix_y land 0x7                                   in
                                      let ptrn_addr    = 4096 + nm_tbl_byte * 16 + fine_y                 in
                                      let ptrn_byte_lo = cc cpu.chr_rom (ptrn_addr + 0)                   in
                                      let ptrn_byte_hi = cc cpu.chr_rom (ptrn_addr + 8)                   in
                                      let attr_addr    = 32 * 30 + 8 * (tile_y_mod lsr 2) + (tile_x lsr 2)    in
                                      let attr_byte    = ( match pix_y with
                                                           | y when y < 240 -> cc cpu.ppu_ram attr_addr
                                                           | y when y >= 256 && y < 496
                                                                           -> cc cpu.ppu_ram (attr_addr + 1024)
                                                           | _ -> 0 )                                     in
                                      let is_rt        = (tile_x land 2) lsr 1                            in
                                      let is_bot       = (tile_y land 2) lsr 1                            in
                                      let shamt        = 2 * (2 * is_bot + is_rt)                         in
                                      let attr_bits    = 3 land (attr_byte lsr shamt)                     in

                                      let bit_col bit  =  (attr_bits lsl 2) lor
                                                          (((ptrn_byte_hi lsr (7-bit)) land 1) lsl 1) lor
                                                          ((ptrn_byte_lo lsr (7-bit)) land 1)             in
                                      let colours      = List.map (fun l -> cpu.plt.( cpu.plt_idx.(bit_col l))) [0; 1; 2; 3; 4; 5; 6; 7] in

                                        List.iter (fun (r,g,b) -> fprintf fd "%3d %3d %3d  " r g b) colours ;

                                        (* printf "%s: x=%d y=%d y_lsr=%d %04X -> %02X\n" file x y (y lsr 4) (32 * (y lsr 4) + x) nm_tbl_byte ; *)
                                   done ;
                                   fprintf fd "\n" ;
                                done ;
                                close_out fd

let ppu_print_vram cpu file = let fd = open_out file in
                                for y = 0 to 63 do
                                   for x = 0 to 31 do
                                      fprintf fd "%02X " (cc cpu.ppu_ram (32 * y + x))
                                   done ;
                                   fprintf fd "\n"
                                done ;
                                close_out fd

let wr_byte cpu addr v = match addr with
                              addr when addr < 2048 -> printf "  MEM WR: %04x <- %02x\n" addr v ;
                                                       Bytes.set_uint8 cpu.ram addr v
                            
                            | addr when addr = 0x2000 -> printf "PPU CTRL: %02x\n" v ; cpu.ppu_ctrl <- v

                            | addr when addr = 0x2006 -> cpu.ppu_wr_addr <- ((cpu.ppu_wr_addr land 0xFF) lsl 8) lor v

                            | addr when addr = 0x2007 -> printf "  PPU WR: %04x <- %02x\n" cpu.ppu_wr_addr v ;
                                                         ppu_ram_wr_hmap cpu v ;
                                                         cpu.ppu_wr_addr <- 0xFFFF land (cpu.ppu_wr_addr + 1) ;
                            | _  -> ()

let stack_ea cpu = cpu.sp lor 0x0100

let rd_byte cpu addr = match addr with
                           addr when addr < 2048           -> cc cpu.ram addr
                         | addr when addr = 0x2002         -> 0x80
                         | addr when addr >= 65536 - 16384 -> cc cpu.rom (0x3FFF land addr)
                         | _  -> 0

let rd_word cpu addr = match addr with
                           addr when addr < 2048           -> Bytes.get_uint16_le cpu.ram addr
                         | addr when addr >= 65536 - 16384 -> Bytes.get_uint16_le cpu.rom (0x3FFF land addr)
                         | _  -> 0

let fetch_param cpu sz = match sz with SZ_1 -> 0
                                     | SZ_2 -> rd_byte cpu (cpu.pc+1)
                                     | SZ_3 -> rd_word cpu (cpu.pc+1)

let se_byte b = let bb = 0xFF land b in
                  if (0x80 land bb) = 0 then bb
                                        else 0xFF00 lor bb

let fetch_ea cpu mode param = match mode with
      Accum   -> None
    | Abs     -> Some param
    | Abs_X   -> Some (param + se_byte cpu.x)
    | Abs_Y   -> Some (param + se_byte cpu.y)
    | Immed   -> None
    | Implied -> None
    | Ind     -> Some (rd_word cpu param)
    | X_Ind   -> Some (rd_word cpu (0xFF land (param + cpu.x)))
    | Ind_Y   -> Some ((rd_word cpu param) + se_byte cpu.y)
    | Zpg     -> Some (0xFF land  param)
    | Zpg_X   -> Some (0xFF land (param + cpu.x))
    | Zpg_Y   -> Some (0xFF land (param + cpu.y))
    | Rel     -> Some (0xFFFF land (cpu.pc + 2 + (se_byte param))) (* param is always single byte for relative addressing *)


let set_acc_nz cpu v = cpu.acc <- v ;
                       cpu.flg_n <- (0x80 land v) lsr 7 ;
                       cpu.flg_z <- if v = 0 then 1 else 0

let cmp_nzc cpu a b = let n_arg = 0xFF land ((0xFF lxor b) + 1) in
                      let sum   = a + n_arg                     in
                     ( cpu.flg_c <- (0x100 land sum) lsr 8 ;
                       cpu.flg_n <- ( 0x80 land sum) lsr 7 ;
                       cpu.flg_z <- if (0xFF land sum) = 0 then 1 else 0 ;)

let set_x_nz cpu v = let arg = 0xFF land v in
                     (  cpu.x     <- arg ;
                        cpu.flg_n <- (0x80 land arg) lsr 7 ;
                        cpu.flg_z <- if arg = 0 then 1 else 0 )

let set_y_nz cpu v = let arg = 0xFF land v in
                        (  cpu.y     <- arg ;
                           cpu.flg_n <- (0x80 land arg) lsr 7 ;
                           cpu.flg_z <- if arg = 0 then 1 else 0 )

let wr_mem_nz cpu addr v = wr_byte cpu addr v;
                           cpu.flg_n <- (0x80 land v) lsr 7;
                           cpu.flg_z <- if v = 0 then 1 else 0

let nmi cpu = let nmi_vect = rd_word cpu 0xFFFA in
                ( wr_byte cpu (stack_ea cpu - 0) ((0xFF00 land (cpu.pc - 1)) lsr 8) ;
                  wr_byte cpu (stack_ea cpu - 1)  (0xFF   land (cpu.pc - 1))  ;
                  wr_byte cpu (stack_ea cpu - 2) (status_byte cpu) ;
                  cpu.sp <- cpu.sp - 3 ;
                  cpu.pc <- nmi_vect ;
                  print_endline "NMI" )

let exec cpu op ea immed = let arg = match ea with 
                                Some (addr) -> rd_byte cpu addr (* FIXME: suppress read on stores*)
                              | None        -> immed
                            in match op with

      ADC -> let sum = cpu.acc + arg + cpu.flg_c in 
               (  cpu.flg_c <- (0x100 land sum) lsr 8 ;
                  cpu.flg_n <-  (0x80 land sum) lsr 7 ;
                  cpu.flg_z <- if cpu.acc = 0 then 1 else 0 ;
                  cpu.flg_v <- if ((cpu.acc land 0x80) = ( arg land 0x80))
                               && ((cpu.acc land 0x80) <> (sum land 0x80)) then 1 else 0 ;
                  cpu.acc   <- 0xFF land sum ; )

    | AND -> set_acc_nz cpu (cpu.acc land arg)
  
    | ASL -> ( match ea with
               Some(addr) -> ( let shifted = arg lsl 1 in (* memory argument *)
                               cpu.flg_c <- (0x100 land shifted) lsr 8 ;
                               wr_mem_nz cpu addr (0xFF land shifted) )

             | None       -> ( let shifted = cpu.acc lsl 1 in (* accumulator *)
                               cpu.flg_c <- (0x100 land shifted) lsr 8;
                               set_acc_nz cpu (0xFF land shifted) ) )

    | BCC -> if cpu.flg_c = 0 then cpu.pc <- (Option.get ea)

    | BCS -> if cpu.flg_c = 1 then cpu.pc <- (Option.get ea)

    | BEQ -> if cpu.flg_z = 1 then cpu.pc <- (Option.get ea)

    | BIT ->   cpu.flg_n <- (0x80 land arg) lsr 7 ;
               cpu.flg_v <- (0x40 land arg) lsr 6 ;
               cpu.flg_z <- if (cpu.acc land arg) = 0 then 1 else 0

    | BMI -> if cpu.flg_n = 1 then cpu.pc <- (Option.get ea)

    | BNE -> if cpu.flg_z = 0 then cpu.pc <- (Option.get ea)

    | BPL -> if cpu.flg_n = 0 then cpu.pc <- (Option.get ea)

    | BRK -> raise Break_Instn
  
    | BVC -> if cpu.flg_v = 0 then cpu.pc <- (Option.get ea)

    | BVS -> if cpu.flg_v = 1 then cpu.pc <- (Option.get ea)

    | CLC -> cpu.flg_c <- 0

    | CLD -> cpu.flg_d <- 0

    | CLI -> cpu.flg_i <- 0

    | CLV -> cpu.flg_v <- 0

    | CMP -> cmp_nzc cpu cpu.acc arg

    | CPX -> cmp_nzc cpu cpu.x arg

    | CPY -> cmp_nzc cpu cpu.y arg

    | DEC -> ( match ea with
                Some(addr) -> wr_mem_nz cpu addr (0xFF land (arg - 1)) 
              | None       -> raise ShouldNeverHappen ) (* ...because INC and DEC have only memory arguments *)
  
    | DEX -> set_x_nz cpu (cpu.x - 1)

    | DEY -> set_y_nz cpu (cpu.y - 1)

    | EOR -> set_acc_nz cpu (cpu.acc lxor arg)

    | INC -> ( match ea with
                Some(addr) -> wr_mem_nz cpu addr (0xFF land (arg + 1)) 
              | None       -> raise ShouldNeverHappen ) (* ...because INC and DEC have only memory arguments *)

    | INX -> set_x_nz cpu (cpu.x + 1)

    | INY -> set_y_nz cpu (cpu.y + 1)

    | JMP ->  cpu.pc <- (Option.get ea)

    | JSR -> ( wr_byte cpu (stack_ea cpu - 0) ((0xFF00 land (cpu.pc - 1)) lsr 8)  ;
               wr_byte cpu (stack_ea cpu - 1)  (0xFF   land (cpu.pc - 1)) ;
               cpu.sp <- cpu.sp - 2 ;
               cpu.pc <- (Option.get ea) )

    | LDA -> set_acc_nz cpu arg

    | LDX -> set_x_nz cpu arg

    | LDY -> set_y_nz cpu arg

    | LSR -> ( match ea with
                 Some(addr) -> ( let shifted = arg lsr 1 in (* memory argument *)
                                 cpu.flg_c <- 1 land arg ;
                                 wr_mem_nz cpu addr shifted )

               | None       -> ( let shifted = cpu.acc lsr 1 in (* accumulator *)
                                 cpu.flg_c <- 1 land cpu.acc ;
                                 set_acc_nz cpu shifted ) )

    | NOP -> ()

    | ORA -> set_acc_nz cpu (cpu.acc lor arg)

    | PHA -> ( wr_byte cpu (stack_ea cpu) cpu.acc ;
               cpu.sp <- cpu.sp - 1 )

    | PHP -> ( wr_byte cpu (stack_ea cpu) (status_byte cpu) ;
               cpu.sp <- cpu.sp - 1 )

    | PLA -> ( cpu.sp <- cpu.sp + 1 ;
               set_acc_nz cpu (rd_byte cpu (stack_ea cpu) ) )

    | PLP -> ( cpu.sp <- cpu.sp + 1 ;
               set_flags cpu (rd_byte cpu (stack_ea cpu) ) )

    | ROL -> ( match ea with
                 Some(addr) -> ( let shifted   = (arg lsl 1) lor cpu.flg_c  in (* memory argument *)
                                 let new_carry = (0x100 land shifted) lsr 8 in
                                 cpu.flg_c <- new_carry ;
                                 wr_mem_nz cpu addr (0xFF land shifted) )

               | None       -> ( let shifted = (cpu.acc lsl 1) lor cpu.flg_c in (* accumulator *)
                                 let new_carry = (0x100 land shifted) lsr 8 in
                                 cpu.flg_c <- new_carry;
                                 set_acc_nz cpu (0xFF land shifted) ) )

    | ROR -> ( match ea with
                   Some(addr) -> ( let shifted = (arg lsr 1) lor (cpu.flg_c lsl 8)  in (* memory argument *)
                                   cpu.flg_c <- arg land 1 ;
                                   wr_mem_nz cpu addr (0xFF land shifted) )

                 | None       -> ( let shifted = (cpu.acc lsr 1) lor (cpu.flg_c lsl 8) in (* accumulator *)
                                   cpu.flg_c <- cpu.acc land 1;
                                   set_acc_nz cpu (0xFF land shifted) ) )

    | RTI -> let rti_flags = rd_byte cpu (stack_ea cpu + 1) in
             let new_pc    = rd_word cpu (stack_ea cpu + 2) in
              ( set_flags cpu rti_flags ;
                cpu.pc <- new_pc + 1;
                cpu.sp <- cpu.sp + 3 )

    | RTS -> let new_pc = rd_word cpu (stack_ea cpu + 1) in
              ( cpu.pc <- new_pc + 1 ;
                cpu.sp <- cpu.sp + 2 )

    | SBC -> let n_arg = 0xFF land ((0xFF lxor arg) + cpu.flg_c) in
             let sum     = cpu.acc + n_arg         in
             (  cpu.flg_c <- (0x100 land sum) lsr 8 ;
                cpu.flg_n <-  (0x80 land sum) lsr 7 ;
                cpu.flg_z <- if (0xFF land sum) = 0 then 1 else 0 ;
                cpu.flg_v <- if ((cpu.acc land 0x80) = ( n_arg land 0x80))
                             && ((cpu.acc land 0x80) <> (sum land 0x80)) then 1 else 0 ;
                cpu.acc   <- 0xFF land sum ; )

    | SEC -> cpu.flg_c <- 1

    | SED -> cpu.flg_d <- 1

    | SEI -> cpu.flg_i <- 1

    | STA -> wr_byte cpu (Option.get ea) cpu.acc (* ...because Stores must have address *)

    | STX -> wr_byte cpu (Option.get ea) cpu.x   (* ...because Stores must have address *)

    | STY -> wr_byte cpu (Option.get ea) cpu.y   (* ...because Stores must have address *)

    | TAX -> set_x_nz cpu cpu.acc

    | TAY -> set_y_nz cpu cpu.acc

    | TSX -> set_x_nz cpu cpu.sp

    | TXA -> set_acc_nz cpu cpu.x

    | TXS -> cpu.sp <- cpu.x
  
    | TYA -> set_acc_nz cpu cpu.y

    | ILL -> raise Illegal_Instn

let step cpu = let Instr (op, mode, sz, cnt) as inst  = decode (rd_byte cpu cpu.pc) in
               let param = fetch_param cpu sz                                       in
               let ea    = fetch_ea cpu mode param                                  in
                printf "%-25s [%s]\n" (show_full_instn inst cpu.pc param) (show_cpu cpu);
                cpu.pc     <- cpu.pc + isize_incr sz ;
                cpu.cycles <- cpu.cycles + cnt ;
                exec cpu op ea param

let run_cycles cpu n = cpu.cycles <- 0 ;
                       while cpu.cycles < n do step cpu done

let main fr path = let (prg, chr) = fetch_segments path      in
                   let plt        = load_palette "FCEUX.pal" in
                   let cpu = mk_machine prg chr plt          in

                   pp_bytes "PRG END:" prg [16384 - 1; 16384 - 2; 16384 - 3; 16384 - 4; 16384 - 5; 16384 - 6; ] ;
                   pp_bytes "PRG STA:" prg [ 0x70; 0x71; 0x72 ] ;
                   pp_bytes "CHR:" chr [0;1;2] ;
                   printf "PC: %x\n" cpu.pc ;
    
                   for i = 0 to 63 do
                    let (r,g,b) = plt.(i) in printf "%d -> (%02x, %02x, %02x)\n" i r g b
                   done;

                   for i = 0 to (int_of_string fr) do
                     printf "FRAME %d\n" i;
                     run_cycles cpu 28333 ;
                     if (cpu.ppu_ctrl land 0x80) <> 0 then nmi cpu ;
                   done;
    
                   ppu_draw_vram cpu "img.ppm";
                   ppu_print_vram cpu "img.txt";
    
                   printf "Done %d cycles. Bye!\n" cpu.cycles ;;

main Sys.argv.(1) Sys.argv.(2) ;;
