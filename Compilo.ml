open Format

let usage = "usage: ./compilo [options] file.cid"

let file =
    let file = ref None in
    let set_file s =
      if not (Filename.check_suffix s ".cid") then
        raise (Arg.Bad "no .cid extension");
      file := Some s
    in
    Arg.parse [] set_file usage;
    match !file with Some f -> f | None -> Arg.usage [] usage; exit 1

let () =
  let c  = open_in file in
  let lb = Lexing.from_channel c in
  let prog = SourceParser.prog SourceLexer.token lb in
  close_in c;
  let _ = SourceTypeChecker.typecheck_program prog in
  let prog = SourceToImp.strip_program prog in
  let prog = ImpToGoto.translate_program prog in
  let prog = IndexedGotoAST.index_program prog in
  let main = (IndexedGotoDeadCodeElim.dead_code_elim prog.main) in
  let globals = prog.globals in
  let strip_fun info = 
    IndexedGotoAST.({
    signature = info.signature;
    locals = info.locals;
    code = IndexedGotoDeadCodeElim.dead_code_elim info.code;
    })
   in
  let functions = prog.functions in
  let prog = IndexedGotoAST.({
      main = main;
      globals = globals;
      functions = CommonAST.Symb_Tbl.map strip_fun functions;
  })
  in
  let prog = IndexedGotoAST.strip_program prog in
  let asm = GotoToMips.translate_program prog in
  let output_file = (Filename.chop_suffix file ".cid") ^ ".asm" in
  let out = open_out output_file in
  let outf = formatter_of_out_channel out in
  Mips.print_program outf asm;
  pp_print_flush outf ();
  close_out out;
  exit 0
