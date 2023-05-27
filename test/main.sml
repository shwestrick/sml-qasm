structure CLA = CommandLineArgs

val inputName =
  case CLA.positional () of
    x :: _ => x
  | _ => Util.die "Error: missing file argument"

(* val circuit =
  ParseQASM.loadFromFile inputName
  handle ParseQASM.ParseError msg => Util.die (inputName ^ ": " ^ msg)

val _ = print (Circuit.toString circuit) *)

(* ======================================================================= *)

structure TCS = TerminalColorString
structure TC = TerminalColors
fun boldc c x =
  TCS.bold (TCS.foreground c (TCS.fromString x))
fun printErr m = TextIO.output (TextIO.stdErr, m)

fun warnWithMessage msg =
  TCS.printErr (boldc Palette.yellow (msg ^ "\n"))

fun failWithMessage msg =
  ( TCS.printErr (boldc Palette.red (msg ^ "\n"))
  ; OS.Process.exit OS.Process.failure
  )

fun handleLexOrParseError exn =
  let
    val e =
      case exn of
        Error.Error e => e
      | other => raise other
    val hist = ExnHistory.history exn
  in
    TCS.print
      (Error.show {highlighter = SOME SyntaxHighlighter.fuzzyHighlight} e);
    if List.null hist then ()
    else print ("\n" ^ String.concat (List.map (fn ln => ln ^ "\n") hist));
    OS.Process.exit OS.Process.failure
  end

fun exnToString exn =
  let
    val header = "UNHANDLED EXCEPTION: " ^ exnMessage exn
    val stackTrace =
      if List.null (ExnHistory.history exn) then
        ""
      else
        "\nSTACK TRACE:\n"
        ^
        List.foldl op^ ""
          (List.map (fn s => "  " ^ s ^ "\n") (ExnHistory.history exn))
  in
    header ^ stackTrace
  end

val _ =
  let
    val source = Source.loadFromFile (FilePath.fromUnixPath inputName)
    val tokens = Lexer.tokens source handle exn => handleLexOrParseError exn
    val ast = Parser.parse tokens handle exn => handleLexOrParseError exn
    val highlighted = SyntaxHighlighter.highlight source
                      handle exn => handleLexOrParseError exn
  in
    TCS.print highlighted;
    print "\n"
  end
  handle exn =>
    ( TCS.printErr (boldc Palette.red (exnToString exn ^ "\n"))
    ; OS.Process.exit OS.Process.failure
    )
