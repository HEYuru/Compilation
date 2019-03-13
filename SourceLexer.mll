{

  open Lexing
  open SourceParser

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ "true",     CONST_BOOL(true);
	"false",    CONST_BOOL(false);
	"while",    WHILE;
	"if",       IF;
	"else",     ELSE;
	"print",    PRINT;
	"main",     MAIN;
	"var",      VAR;
        "integer",  INTEGER;
        "boolean",  BOOLEAN;
        "new",      NEW;
        "struct",   STRUCT;
        (* Nouveau mot-clé *)
        "return",   RETURN;
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)
        
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | '\'' | digit)*
  
rule token = parse
  | [' ' '\t']+
      { token lexbuf }
  | ['\n']
      { new_line lexbuf; token lexbuf }
  | "//"
      { comment lexbuf; token lexbuf }
  | digit+
      { CONST_INT (int_of_string (lexeme lexbuf)) }
  | ident
      { id_or_keyword (lexeme lexbuf) }
  | "{"
      { BEGIN }
  | "}"
      { END }
  | "("
      { LP }
  | ")"
      { RP }
  | ";"
      { SEMI }
  | ":="
      { SET }
  | "+"
      { PLUS }
  | "-"
      { MINUS }
  | "*"
      { STAR }
  | "/"
      { DIV }
  | "%"
      { MOD }
  | "=="
      { EQUAL }
  | "!="
      { NEQ }
  | "<"
      { LT }
  | "<="
      { LE }
  | ">"
      { GT }
  | ">="
      { GE }
  | "&&"
      { AND }
  | "||"
      { OR }
  | "!"
      { NOT }
  | "["
      { LB }
  | "]"
      { RB }
  | "."
      { DOT }
  (* Nouveau symbole pour la séparation des paramètres *)
  | ","
      { COMMA }
  | _
      { failwith ("Unknown character : " ^ (lexeme lexbuf)) }
  | eof
      { EOF }

and comment = parse
  | [^ '\n']*
      { comment lexbuf }
  | '\n'
      { new_line lexbuf }
  | eof
      { () }
    
