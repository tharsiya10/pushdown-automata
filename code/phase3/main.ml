match Sys.argv with
  | [| _; "-print"; file |] -> 
    let file_channel = open_in file in
    let lexbuf = Lexing.from_channel file_channel in
    Printf.printf "Parse:\n%s\n" (Ast.as_string_automate (Parser.input Lexer.main lexbuf));
    true
  | [|_;"-exec";file;mot|] -> 
    let file_channel = open_in file in 
    let lexbuf = Lexing.from_channel file_channel in
    Interpreteur.execute (Parser.input Lexer.main lexbuf) mot
  | _ -> failwith "./parser -print <fichier>\nou\n./parser -exec <fichier> <mot>"