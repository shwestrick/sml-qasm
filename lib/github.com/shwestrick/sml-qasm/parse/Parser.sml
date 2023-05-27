(** Copyright (c) 2023 Sam Westrick                                            
  *                                                                                 
  * See the file LICENSE for details.                                            
  *)

structure Parser:
sig
  val parse: Token.t Seq.t -> Ast.t
end =
struct
  structure PC = ParserCombinators
  structure PS = ParseSimple

  type ('a, 'b) parser = ('a, 'b) PC.parser
  type 'a peeker = 'a PC.peeker

  fun parse allTokens =
    let
      val toks = Seq.filter (not o Token.isCommentOrWhitespace) allTokens
      val numToks = Seq.length toks
      fun tok i = Seq.nth toks i

      (** not yet implemented *)
      fun nyi fname i =
        ParserUtils.nyi toks fname i

      (** This silliness lets you write almost-English like this:                
        *   if is Token.Identifier at i           then ...                       
        *   if isReserved Token.Val at i          then ...                       
        *   if check isTyVar at i                 then ...                       
        *)
      infix 5 at
      fun f at i = f i
      fun check f i =
        i < numToks andalso f (tok i)
      fun is c =
        check (fn t => c = Token.getClass t)
      fun isReserved rc =
        check (fn t => Token.Reserved rc = Token.getClass t)

      fun parse_reserved rc i =
        PS.reserved toks rc i
      fun parse_maybeReserved rc i =
        PS.maybeReserved toks rc i

      fun parse_oneOrMoreDelimitedByReserved x i =
        PC.oneOrMoreDelimitedByReserved toks x i
      fun parse_two (p1, p2) state =
        PC.two (p1, p2) state
      fun parse_zeroOrMoreWhile c p s =
        PC.zeroOrMoreWhile c p s
      fun parse_oneOrMoreWhile c p s =
        PC.oneOrMoreWhile c p s


      (* =================================================================== *)


      fun parse_stmt i = nyi "parse_stmt" i


      (*  OPENQASM version ;
       * ^         
       *)
      fun parse_maybeVersion i =
        if not (isReserved Token.Openqasm i) then
          (i, NONE)
        else
          let
            val (i, openqasm) = (i + 1, tok i)
            val (i, version) =
              if check Token.isVersionIdentifier i then
                (i + 1, tok i)
              else
                ParserUtils.tokError toks
                  { pos = i
                  , what = "Unexpected token."
                  , explain = SOME "Expected version identifier."
                  }

            val (i, semicolon) = parse_reserved Token.Semicolon i
          in
            ( i
            , SOME
                {openqasm = openqasm, version = version, semicolon = semicolon}
            )
          end


      fun parse_ast i =
        let
          val (i, version) = parse_maybeVersion i

          val (i, stmts) =
            parse_zeroOrMoreWhile (fn i => i < numToks) parse_stmt i
        in
          Ast.Ast {version = version, stmts = stmts}
        end

    in
      parse_ast 0
    end

end
