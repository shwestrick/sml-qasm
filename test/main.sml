structure CLA = CommandLineArgs

val inputName =
  case CLA.positional () of
    x :: _ => x
  | _ => Util.die "Error: missing file argument"

val circuit =
  ParseQASM.loadFromFile inputName
  handle ParseQASM.ParseError msg => Util.die (inputName ^ ": " ^ msg)

val _ = print (Circuit.toString circuit)
