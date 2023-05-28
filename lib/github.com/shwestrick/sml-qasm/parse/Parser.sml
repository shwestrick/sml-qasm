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

      fun parse_identifier i = PS.identifier toks i
      fun parse_reserved rc i =
        PS.reserved toks rc i
      fun parse_maybeReserved rc i =
        PS.maybeReserved toks rc i

      fun parse_oneOrMoreDelimitedByReserved x i =
        PC.oneOrMoreDelimitedByReserved toks x i
      fun parse_zeroOrMoreDelimitedByReserved x i =
        PC.zeroOrMoreDelimitedByReserved toks x i
      fun parse_two (p1, p2) state =
        PC.two (p1, p2) state
      fun parse_zeroOrMoreWhile c p s =
        PC.zeroOrMoreWhile c p s
      fun parse_oneOrMoreWhile c p s =
        PC.oneOrMoreWhile c p s


      (* =================================================================== *)


      fun parse_exp i =
        if check Token.isIdentifier i then (i + 1, Ast.Exp.Identifier (tok i))
        else if check Token.isLiteral i then (i + 1, Ast.Exp.Literal (tok i))
        else nyi "parse_exp" i


      fun parse_maybeDesignator i =
        if not (isReserved Token.OpenBracket i) then
          (i, NONE)
        else
          let
            val (i, lbracket) = (i + 1, tok i)
            val (i, exp) = parse_exp i
            val (i, rbracket) = parse_reserved Token.CloseBracket i
          in
            (i, SOME {lbracket = lbracket, exp = exp, rbracket = rbracket})
          end


      fun parse_maybeParams i =
        if not (isReserved Token.OpenParen i) then
          (i, NONE)
        else
          let
            val (i, lparen) = (i + 1, tok i)
            val (i, {elems, delims}) =
              parse_zeroOrMoreDelimitedByReserved
                { parseElem = parse_exp
                , delim = Token.Comma
                , shouldStop = isReserved Token.CloseParen
                } i
            val (i, rparen) = parse_reserved Token.CloseParen i
          in
            ( i
            , SOME
                { lparen = lparen
                , elems = elems
                , delims = delims
                , rparen = rparen
                }
            )
          end


      fun parse_operand i =
        let
          val (i, name) = parse_identifier i
          val (i, index) = parse_maybeDesignator i
        in
          (i, Ast.Stmt.IndexedIdentifier {name = name, index = index})
        end


      fun parse_operands i =
        let
          fun stop i =
            isReserved Token.Semicolon i
            orelse
            (isReserved Token.Comma i andalso isReserved Token.Semicolon (i + 1))

          val (i, args) =
            parse_zeroOrMoreDelimitedByReserved
              { parseElem = parse_operand
              , delim = Token.Comma
              , shouldStop = stop
              } i
        in
          (i, args)
        end


      (* name [(exp, ...)] operand, ...
       *     ^
       *)
      fun parse_gateCall i =
        let
          val name = tok (i - 1)
          val (i, params) = parse_maybeParams i
          val (i, args) = parse_operands i
          val (i, semicolon) = parse_reserved Token.Semicolon i
        in
          ( i
          , Ast.Stmt.GateCall
              {name = name, params = params, args = args, semicolon = semicolon}
          )
        end


      (* include "path..." ;
       *        ^ 
       *)
      fun parse_include i =
        let
          val includee = tok (i - 1)
          val (i, path) =
            if check Token.isStringLiteral i then
              (i + 1, tok i)
            else
              ParserUtils.tokError toks
                {pos = i, what = "Expected string literal.", explain = NONE}
          val (i, semicolon) = parse_reserved Token.Semicolon i
        in
          ( i
          , Ast.Stmt.Include
              {includee = includee, path = path, semicolon = semicolon}
          )
        end


      (* reg id designator? ;
       *    ^ 
       * e.g.
       *   qreg x[5];
       *   creg y;
       *)
      fun parse_oldStyleDeclare i =
        let
          val reg = tok (i - 1)
          val (i, ident) = parse_identifier i
          val (i, designator) = parse_maybeDesignator i
          val (i, semicolon) = parse_reserved Token.Semicolon i
        in
          ( i
          , Ast.Stmt.OldStyleDeclare
              { reg = reg
              , ident = ident
              , designator = designator
              , semicolon = semicolon
              }
          )
        end


      (* measure operand [-> operand] ;
       *        ^ 
       *)
      fun parse_measure i =
        let
          val measure = tok (i - 1)
          val (i, arg) = parse_operand i
          val (i, into) =
            if not (isReserved Token.Arrow i) then
              (i, NONE)
            else
              let
                val (i, arrow) = (i + 1, tok i)
                val (i, arg) = parse_operand i
              in
                (i, SOME {arrow = arrow, arg = arg})
              end

          val (i, semicolon) = parse_reserved Token.Semicolon i
        in
          ( i
          , Ast.Stmt.Measure
              {measure = measure, arg = arg, into = into, semicolon = semicolon}
          )
        end


      (* barrier [operand, ...] ;
       *        ^ 
       *)
      fun parse_barrier i =
        let
          val barrier = tok (i - 1)

          val (i, {elems, delims}) = parse_operands i
          val (i, semicolon) = parse_reserved Token.Semicolon i
        in
          ( i
          , Ast.Stmt.Barrier
              { barrier = barrier
              , elems = elems
              , delims = delims
              , semicolon = semicolon
              }
          )
        end


      fun parse_reset i =
        let
          val reset = tok (i - 1)
          val (i, arg) = parse_operand i
          val (i, semicolon) = parse_reserved Token.Semicolon i
        in
          (i, Ast.Stmt.Reset {reset = reset, arg = arg, semicolon = semicolon})
        end


      fun parse_stmt i =
        if isReserved Token.Include i then
          parse_include (i + 1)
        else if isReserved Token.Qreg i orelse isReserved Token.Creg i then
          parse_oldStyleDeclare (i + 1)
        else if isReserved Token.Measure i then
          parse_measure (i + 1)
        else if isReserved Token.Barrier i then
          parse_barrier (i + 1)
        else if isReserved Token.Reset i then
          parse_reset (i + 1)
        else if check Token.isIdentifier i then
          parse_gateCall (i + 1)
        else
          nyi "parse_stmt" i


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
                  , what = "Invalid version identifier."
                  , explain = NONE
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
