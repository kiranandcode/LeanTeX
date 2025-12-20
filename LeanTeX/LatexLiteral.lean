import Lean
open Lean Parser

private def getString (input: ParserContext) (substring: String) : ParserState -> ParserState :=
  fun s => Id.run $ do
    let mut s := s
    let mut curr := input.get s.pos
    for char in substring.toList do
       if input.atEnd s.pos then
         return s.mkEOIError
       curr  := input.get s.pos
       if curr != char then
         return s.mkError "interpolated latex literal"
       s := s.next input s.pos

    return s

private def leanTexStart (s: ParserState) (input: ParserContext) :=
  let curr := input.get s.pos
  let s    := s.setPos (input.next s.pos)
  if curr == '['
  then
    let curr := input.get s.pos
    let s := s.setPos (input.next s.pos)
    if curr == '|'
    then s
    else s.mkError "interpolated latex literal start [|"
  else s.mkError "interpolated latex literal start [|"


private def leanTexEnd s input := getString input "|]" s

@[inline]
partial def interpolatedLatexFn : ParserFn := fun c s => Id.run $ do
  let p := Parser.termParser.fn
  let input     := c
  let stackSize := s.stackSize
  let rec parse (step: Nat) (startPos : String.Pos.Raw) (c : ParserContext) (s : ParserState) : ParserState :=
    let parse := parse (step + 1)
    let i     := s.pos
    -- if step == 19
    -- then s.mkError  s!"parsed {input.extract startPos s.pos} '{input.get s.pos}'"
    -- else
    if input.atEnd i then
      let s := s.mkError s!"unterminated latex literal {input.extract startPos i}"
      s.mkNode interpolatedStrLitKind stackSize
    else
      let curr := input.get s.pos
      let s    := s.setPos (input.next s.pos)
      if curr == '|' then
         let curr := input.get s.pos
         let s    := s.setPos (input.next s.pos)
         if curr == ']' then
            let s := (mkNodeToken interpolatedStrLitKind startPos) c s
            s.mkNode interpolatedStrKind stackSize
         else
            parse startPos c s
      else if curr == '[' then
        let curr := input.get s.pos
        let s    := s.setPos (input.next s.pos)
        if curr == '|' then
          let s := (mkNodeToken interpolatedStrLitKind startPos) c s
          let s := p c s
          if s.hasError then s
          else
            let curr := input.get s.pos
            let s := s.setPos (input.next s.pos)
            if curr == '|' then
              let curr := input.get s.pos
              if curr == ']' then
                let s := s.setPos (input.next s.pos)
                parse s.pos c s
              else
                let s := s.mkError s!"'|]', found {curr}"
                s.mkNode interpolatedStrKind stackSize
            else
              let s := s.mkError s!"'|]' found {curr}"
              s.mkNode interpolatedStrKind stackSize
        else parse startPos c s
      else if curr == '@' then
        let curr := input.get s.pos
        let s    := s.setPos (input.next s.pos)
        if curr == '{' then
          let sPrev := s.setPos i
          let s := (mkNodeToken interpolatedStrLitKind startPos) c sPrev
          let s := s.setPos (input.next (input.next i))
          let s := p c s
          if s.hasError then s
          else
            let curr := input.get s.pos
            if curr == '}' then
                let s := s.setPos (input.next s.pos)
                parse s.pos c s
            else
              let s := s.mkError s!"'}' found {curr}"
              s.mkNode interpolatedStrKind stackSize
        else parse startPos c s
      else
        parse startPos c s
  let startPos := s.pos
  if input.atEnd startPos then
    s.mkEOIError
  else
    let s := leanTexStart s input
    if s.hasError then s
    else
      parse 0 startPos c s


def interpolatedLatexParser : Parser :=
 withAntiquot (mkAntiquot "interpolatedStr" interpolatedStrKind) $
  Parser.mk (mkAtomicInfo "interpolatedStr") interpolatedLatexFn


@[combinator_formatter interpolatedLatexParser]
def interpolatedLatexParserFormatter : Lean.PrettyPrinter.Formatter := do return ()

@[combinator_parenthesizer interpolatedLatexParser]
def interpolatedLatexParserParenthesizer : Lean.PrettyPrinter.Parenthesizer := do return ()

partial def decodeInterpLatexLit (val: String) : Option String :=
    let try_ (f: String -> Option Substring.Raw) (s: String) : String :=
       match f s with
       | .none => s
       | .some s => s.toString
    val
    |> try_ (·.dropPrefix? "[|")
    |> try_ (·.dropPrefix? "|]")
    |> try_ (·.dropSuffix? "[|")
    |> try_ (·.dropSuffix? "|]")

partial def isInterpolatedLatexLit? (stx : Syntax) : Option String :=
  match Syntax.isLit? interpolatedStrLitKind stx with
  | none     => none
  | some val => decodeInterpLatexLit val

def expandInterpolatedLatexChunks (chunks : Array Syntax) (mkAppend : Syntax → Syntax → MacroM Syntax) (mkElem : Syntax → MacroM Syntax) : MacroM Syntax := do
  let mut i := 0
  let mut result := Syntax.missing
  for elem in chunks do
    let elem ← match isInterpolatedLatexLit? elem with
      | none     => mkElem elem
      | some str => mkElem (Syntax.mkStrLit str)
    if i == 0 then
      result := elem
    else
      result ← mkAppend result elem
    i := i+1
  return result

open TSyntax.Compat in
def expandInterpolatedLatex (interpStr : TSyntax interpolatedStrKind) (type : Term) (toTypeFn : Term) : MacroM Term := do
  let r ← expandInterpolatedLatexChunks interpStr.raw.getArgs (fun a b => `($a ++ $b)) (fun a => `($toTypeFn $a))
  `(($r : $type))
