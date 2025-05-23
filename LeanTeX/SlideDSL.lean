import Lean
import LeanTeX.Slide
import LeanTeX.LatexLiteral
import LeanTeX.PackageRegistry
import LeanTeX.PreambleRegistry
open Lean Elab Command Term Meta

declare_syntax_cat slide_item
declare_syntax_cat tikz_item
declare_syntax_cat interval
declare_syntax_cat interval_spec
declare_syntax_cat tikz_at_spec
declare_syntax_cat latex_option

syntax (name := slide) "slide " (Parser.strLit)? " do " ppLine withPosition((colEq slide_item ppLine)+) : term

syntax 
   "\\begin" "{" ident "}" (colGt slide_item)*
   "\\end" "{" ident "}" : slide_item
syntax "\\item" "{" term  "}" : slide_item
syntax "latex!"interpolatedLatexParser : slide_item

syntax ident : latex_option
syntax ident " = " term : latex_option
syntax "latex!"interpolatedLatexParser : latex_option
syntax str : latex_option
syntax "s!"interpolatedStr(term) : latex_option
syntax "{" term "}" : latex_option

def expandLatexOptionStr : TSyntax `latex_option -> MacroM String
| `(latex_option| $id:ident ) => do
   let id := id.getId.toString
   return id
| `(latex_option| $id:ident = $e:str ) => do
   let id := id.getId.toString
   return (id ++ "=" ++ e.getString)
| `(latex_option| $s:str ) => return s.getString
| stx => Macro.throwErrorAt stx "Expected literal LaTeX option for package"


def expandLatexOption : TSyntax `latex_option -> MacroM (TSyntax `term)
| `(latex_option| $id:ident ) => do
   let id := id.getId.toString
   let stx := Syntax.mkStrLit id
   return stx
| `(latex_option| $id:ident = $e:term ) => do
   let id := id.getId.toString
   let stx := Syntax.mkStrLit id
   `($stx ++ "=" ++ $e)
| `(latex_option| latex!$str:interpolatedStr) => do
   expandInterpolatedLatex str (← `(String)) (← `(toString))
| `(latex_option| s!$str:interpolatedStr) => do
   TSyntax.expandInterpolatedStr str (← `(String)) (← `(toString))
| `(latex_option| $s:str ) => return s
| `(latex_option| { $t:term } ) => `(term| ToString.toString $t)
| stx => Macro.throwErrorAt stx "Unexpected LaTeX option format"

syntax ident : interval
syntax ident "-" (ident)? : interval
syntax "-" ident : interval
syntax "<" interval,+ ">" : interval_spec
syntax "<" ident"->" : interval_spec
syntax "<-" ident">" : interval_spec

def expandInterval : TSyntax `interval -> MacroM (TSyntax `term)
| `(interval| $id:ident) => `(LatexInterval.at $id)
| `(interval| $id:ident - ) => `(LatexInterval.after $id)
| `(interval| $st:ident - $ed:ident ) => `(LatexInterval.between $st $ed)
| `(interval| - $id:ident ) => `(LatexInterval.upto $id)
| stx => Macro.throwErrorAt stx "Unsupported Interval form"

syntax " at " "(" term "," term ")" : tikz_at_spec
syntax " at " "(" term ")" : tikz_at_spec
syntax " at " ident : tikz_at_spec
syntax " at " str : tikz_at_spec

def expandTikzAtSpec : TSyntax `tikz_at_spec -> MacroM (TSyntax `term)
| `(tikz_at_spec| at ($t1:term , $t2:term)) => `(ToString.toString ($t1, $t2))
| `(tikz_at_spec| at ($t1:term)) => `(ToString.toString $t1)
| `(tikz_at_spec| at $id:ident) => `(ToString.toString $id)
| `(tikz_at_spec| at $id:str) => `($id)
| stx => Macro.throwErrorAt stx "Illegal Tikz at form"


declare_syntax_cat tikz_body
syntax "latex!"interpolatedLatexParser : tikz_body
syntax term ppLine : tikz_body

def expandTikzBody : TSyntax `tikz_body -> MacroM (TSyntax `term)
| `(tikz_body| latex!$str) => do
   let s <- expandInterpolatedLatex str (← `(String)) (← `(toString))
   return s
| `(tikz_body| $s:term) => `(term| ToString.toString $s)
| stx => Macro.throwErrorAt stx "Illegal Tikz body form"

declare_syntax_cat tikz_coordinate_binding
syntax "(" term "," term ")" " := " interpolatedLatexParser : tikz_coordinate_binding
syntax "(" term "," term ")" " := " str : tikz_coordinate_binding

syntax "with " " coordinates " tikz_coordinate_binding,* " in "  "node" (interval_spec)? ("[" latex_option,* "]")? (ident)? (tikz_at_spec)? tikz_body: tikz_item

syntax "node" (interval_spec)? ("[" latex_option,* "]")? (ident)? (tikz_at_spec)? tikz_body : tikz_item
syntax "draw" (interval_spec)? ("[" latex_option,* "]")? tikz_body : tikz_item
syntax "on " "layer " ident " do " ppLine withPosition((colEq tikz_item ppLine)*): tikz_item
syntax "for " sepBy1(Parser.Term.doForDecl, ", ") " do " ppLine withPosition((colEq tikz_item ppLine)*): tikz_item


def expandIntervalSpec: TSyntax `interval_spec -> MacroM (TSyntax `term)
| `(interval_spec| < $it:interval,* > ) => do
  let elems <- it.getElems.mapM expandInterval
  `([$elems,*])
| `(interval_spec| <-$id:ident> ) => do
  `([LatexInterval.upto $id])
| `(interval_spec| <$id:ident-> ) => do
  `([LatexInterval.after $id])
| stx => Macro.throwErrorAt stx "Illegal interval structure"

def elabTikzCoordinateBindingExpr : TSyntax `tikz_coordinate_binding -> MacroM (TSyntax `term)
| `(tikz_coordinate_binding| ($x, $y) := $str:str) => do
   return str
| `(tikz_coordinate_binding| ($x, $y) := $str:interpolatedStr) => do
   let s <- expandInterpolatedLatex str (← `(String)) (← `(toString))
   return s
| stx => Macro.throwErrorAt stx "Illegal coordinate binding form [121]"

def elabTikzCoordinateBindingFold (n: Nat) (rest: TSyntax `term) : TSyntax `tikz_coordinate_binding -> MacroM (Nat × TSyntax `term)
| `(tikz_coordinate_binding| ($x, $y) := $str:str) => do
   let xn := Syntax.mkStrLit s!"\\x{n}"
   let yn := Syntax.mkStrLit s!"\\y{n}"
   let binding <- `(term| let ($x,$y) := ($xn, $yn); $rest)
   return (n + 1, binding)
| `(tikz_coordinate_binding| ($x, $y) := $str:interpolatedStr) => do
   let xn := Syntax.mkStrLit s!"\\x{n}"
   let yn := Syntax.mkStrLit s!"\\y{n}"
   let binding <- `(term| let ($x,$y) := ($xn, $yn); $rest)
   return (n + 1, binding)
| stx => Macro.throwErrorAt stx "Illegal coordinate binding form [129]"


partial def elabTikzItem: TSyntax `tikz_item -> MacroM (TSyntax `term)
| `(tikz_item| on layer $id:ident do
$[
$items:tikz_item
]*
) => do
  let elts <- items.mapM elabTikzItem
  let idStx := Syntax.mkStrLit id.getId.toString
  `(TikzCommand.layer $idStx [$elts,*])
| `(tikz_item| for $binding,* do
$[
$items:tikz_item
]*
) => do
  let res <- mkFreshIdent (<- `(res))
  let elts <- items.mapM elabTikzItem
  let elts : Array (TSyntax `Lean.Parser.Term.doReassign) <-
     elts.mapM fun elt =>
      `(Lean.Parser.Term.doReassign|$res := List.cons $elt $res)
  let elts : Array (TSyntax `Lean.Parser.Term.doSeqItem) <-
     elts.mapM fun (elt: TSyntax `Lean.Parser.Term.doReassign) =>
      `(Lean.Parser.Term.doSeqItem| $elt:doReassign)
  let elts : TSyntax `Lean.Parser.Term.doSeq <-
     `(Lean.Parser.Term.doSeq| $[$elts]*)
  `(TikzCommand.block (
  Id.run $ do
    let mut $res := []
    for $binding,* do
       $elts
    return (List.reverse $res)
  ))

| `(tikz_item| draw $(it)? [$opt:latex_option,*] $s:tikz_body) => do
   let it <- if let .some it := it then expandIntervalSpec it else `(term| [])
   let opts <- opt.getElems.mapM expandLatexOption
   let s <- expandTikzBody s
   `(TikzCommand.draw $it [$opts,*] $s)

| `(tikz_item| draw $(it)? $s:tikz_body) => do
   let it <- if let .some it := it then expandIntervalSpec it else `(term| [])
   let s <- expandTikzBody s
   `(TikzCommand.draw $it [] $s)

| `(tikz_item| with coordinates $t,* in node $(it)? [$opt:latex_option,*] $(name)? $(at_spec)? $s:tikz_body) => do
   let it <- if let .some it := it then expandIntervalSpec it else `(term| [])
   let opts <- opt.getElems.mapM expandLatexOption
   let s <- expandTikzBody s
   let at_spec <- if let .some at_spec := at_spec
                  then `(term| Option.some $(<- expandTikzAtSpec at_spec))
                  else `(term| Option.none)
   let name <-
      if let .some name := name
      then `(term| Option.some $(Syntax.mkStrLit name.getId.toString))
      else `(term| Option.none)
   let bindings <- t.getElems.mapM elabTikzCoordinateBindingExpr
   let rest <- `(TikzCommand.pathLetNode [$bindings,*] $it [$opts,*] $at_spec $name $s)
   let (_, res) <- t.getElems.foldlM (init:=(1, rest)) (fun (n, rest) tstx => elabTikzCoordinateBindingFold n rest tstx)
   return res
| `(tikz_item| with coordinates $t,* in node $(it)? $(name)? $(at_spec)? $s:tikz_body) => do
   let it <- if let .some it := it then expandIntervalSpec it else `(term| [])
   let s <- expandTikzBody s
   let at_spec <- if let .some at_spec := at_spec
                  then `(term| Option.some $(<- expandTikzAtSpec at_spec))
                  else `(term| Option.none)
   let name <-
      if let .some name := name
      then `(term| Option.some $(Syntax.mkStrLit name.getId.toString))
      else `(term| Option.none)
   let bindings <- t.getElems.mapM elabTikzCoordinateBindingExpr
   let rest <- `(TikzCommand.pathLetNode [$bindings,*] $it [] $at_spec $name $s)
   let (_, res) <- t.getElems.foldrM (init:=(1, rest)) (fun tstx (n, rest) => elabTikzCoordinateBindingFold n rest tstx)
   return res
| `(tikz_item| node $(it)? [$opt:latex_option,*] $(name)? $(at_spec)? $s:tikz_body) => do
   let it <- if let .some it := it then expandIntervalSpec it else `(term| [])
   let opts <- opt.getElems.mapM expandLatexOption
   let s <- expandTikzBody s
   let at_spec <- if let .some at_spec := at_spec
                  then `(term| Option.some $(<- expandTikzAtSpec at_spec))
                  else `(term| Option.none)
   let name <-
      if let .some name := name
      then `(term| Option.some $(Syntax.mkStrLit name.getId.toString))
      else `(term| Option.none)
   `(TikzCommand.node $it [$opts,*] $at_spec $name $s)
 | `(tikz_item| node $(it)? $(name)? $(at_spec)? $s:tikz_body) => do
   let it <- if let .some it := it then expandIntervalSpec it else `(term| [])
   let s <- expandTikzBody s
   let at_spec <- if let .some at_spec := at_spec
                  then `(term| Option.some $(<- expandTikzAtSpec at_spec))
                  else `(term| Option.none)
   let name <-
      if let .some name := name
      then `(term| Option.some $(Syntax.mkStrLit name.getId.toString))
      else `(term| Option.none)
   `(TikzCommand.node $it [] $at_spec $name $s)
| stx => Macro.throwErrorAt stx "Illegal Tikz Item form"


syntax "tikz" ("[" latex_option,* "]")? " do " ppLine withPosition((colEq tikz_item ppLine)*): slide_item


syntax "with " "steps " "[" ident,* "]" " do " ppLine withPosition((colEq slide_item ppLine)*) : slide_item
syntax term  : slide_item

syntax "with " "steps " "[" ident,* "]" term : term

macro_rules
| `(with steps [$names:ident,*] $expr:term) => do
  let expr <-
    names.getElems.zipIdx.foldlM
     (fun (acc: TSyntax `term) (name, idx) =>
       let idLit := Syntax.mkNatLit (idx + 1)
       `(term| let $name := $idLit:term
               $acc:term))
     (TSyntax.mk expr)
  return expr

partial def elabItem : TSyntax `slide_item -> Elab.Term.TermElabM (TSyntax `term)
| `(slide_item| tikz [$opts:latex_option,*] do
$[
$elts:tikz_item
]*) => do
   let elts <- liftMacroM $ elts.mapM fun elt => elabTikzItem elt
   let opts <- liftMacroM $ opts.getElems.mapM fun opt => expandLatexOption opt
   let contentStx <-
     `(term| TikzPictureContext.mk Std.HashMap.emptyWithCapacity [$opts,*])
   `(term| SlideContent.tikzpicture $contentStx [$elts,*])
| `(slide_item| tikz do
$[
$elts:tikz_item
]*) => do
   let elts <- liftMacroM $ elts.mapM fun elt => elabTikzItem elt
   let contentStx <- `(term| TikzPictureContext.mk Std.HashMap.emptyWithCapacity [])
   `(term| SlideContent.tikzpicture $contentStx [$elts,*])
| `(slide_item| with steps [$names:ident,*] do
$[
$elts:slide_item
]*) => do
   let elts <- elts.mapM fun elt => elabItem elt
   let elts <- `(SlideContent.block [$elts,*])
   let expr <-
     names.getElems.zipIdx.foldlM
     (fun (acc: TSyntax `term) (name, idx) =>
       let idLit := Syntax.mkNatLit (idx + 1)
       `(term| let $name := $idLit:term
               $acc))
        elts
   return expr
| `(slide_item| latex!$txt:interpolatedStr) => do
    let stx <- liftMacroM $ expandInterpolatedLatex txt (← `(String)) (← `(toString))
    `(SlideContent.text $stx)
| `(slide_item| \item{$txt}) => `(SlideContent.item $txt)
| `(slide_item| \begin{$t1:ident} $[$elts:slide_item]* \end{$t2:ident}) => do
    if t1.getId != t2.getId then
       throwErrorAt t1 "found block with mismatched tags {t1} != {t2}" 
    let tag := Syntax.mkStrLit t1.getId.toString
    let elts <- elts.mapM fun elt => elabItem elt
    `(SlideContent.environment $tag:term [$elts,*])
| `(slide_item|  $t:term
) => pure t
| _ => fun _ => Elab.throwUnsupportedSyntax

elab_rules : term
| `(slide $s:str do
       $[
       $body:slide_item
       ]*
) => do
   let body <- body.mapM elabItem
   let body := Syntax.TSepArray.ofElems body 
   let stx <- `(Slide.BasicSlide [] (.some $s) [$body,*])
   elabTerm stx .none
| `(slide do
$[
$body:slide_item
]*) => do
   let body <- body.mapM elabItem
   let body := Syntax.TSepArray.ofElems body 
   let stx <- `(Slide.BasicSlide [] .none [$body,*])
   elabTerm stx .none


declare_syntax_cat usepackage_name
syntax ident: usepackage_name
syntax str: usepackage_name
syntax "#usepackage" usepackage_name ("with " "options " "[" latex_option,* "] " )? : command

def expandUsePackageName : TSyntax `usepackage_name -> MacroM String
| `(usepackage_name| $id:ident ) => do
   let id := id.getId.toString
   return id
| `(usepackage_name| $id:str ) => do return id.getString
| stx => Macro.throwErrorAt stx "Unexpected usepackage form"

elab_rules : command
| `(#usepackage $name:usepackage_name with options [$opt:latex_option,*] ) => do
   let name <- liftMacroM $ expandUsePackageName name
   let opts <- liftMacroM $ opt.getElems.mapM expandLatexOptionStr
   liftTermElabM <| LeanTeX.addPackage name opts.toList
| `(#usepackage $usepackage_name) => do
   let name <- liftMacroM $ expandUsePackageName usepackage_name
   let opts := []
   liftTermElabM <| LeanTeX.addPackage name opts

syntax "#latex_preamble" interpolatedLatexParser : command

elab_rules : command
| `(#latex_preamble $str:interpolatedStr) => do
    let stx <-
      liftMacroM $
         expandInterpolatedLatex str (← `(String)) (← `(toString))
    let binding <- liftTermElabM <| mkFreshUserName `preambleSnippet
    let bindingStx := mkIdent binding
    elabCommand <| (<- `(def $bindingStx:ident := $stx))
    liftTermElabM <| LeanTeX.addPreambleSnippet binding
    

syntax "#latex_slide_expr" term : command
syntax "#latex_slide " ("[" latex_option,* "]")? (Parser.strLit)? " do " (colGt slide_item linebreak)* : command
syntax "#latex_command " interpolatedLatexParser : command

elab_rules : command
| `(#latex_command $str:interpolatedStr) => do
    let stx <-
      liftMacroM $
         expandInterpolatedLatex str (← `(String)) (← `(toString))
    let _binding <- liftTermElabM <| mkFreshUserName `slide
    let bindingStx := mkIdent _binding
    let stx <- `(term| Slide.RawSlide $stx)
    elabCommand <| (<- `(@[presentation]def $bindingStx:ident := $stx))

elab_rules : command
| `(#latex_slide_expr $expr:term) => do
    let binding <- liftTermElabM <| mkFreshUserName `slide
    let bindingStx := mkIdent binding
    elabCommand <| (<- `(@[presentation]def $bindingStx:ident := $expr))
| `(#latex_slide [$opt,*] $t:str do $[$body:slide_item
]*) => do
    let opts <- liftMacroM <| opt.getElems.mapM expandLatexOption
    let binding <- liftTermElabM <| mkFreshUserName `slide
    let bindingStx := mkIdent binding
 
    let body <- liftTermElabM $ body.mapM elabItem
    let body := Syntax.TSepArray.ofElems body 
    let stx <- `(term| Slide.BasicSlide [$opts,*] (.some $t) [$body,*])
    elabCommand <|
      (<- `(@[presentation]def $bindingStx:ident := $stx:term))
| `(#latex_slide $t:str do $[$body:slide_item
]*) => do
    let binding <- liftTermElabM <| mkFreshUserName `slide
    let bindingStx := mkIdent binding
 
    let body <- liftTermElabM $ body.mapM elabItem
    let body := Syntax.TSepArray.ofElems body 
    let stx <- `(term| Slide.BasicSlide [] (.some $t) [$body,*])
    elabCommand <|
      (<- `(@[presentation]def $bindingStx:ident := $stx:term))

| `(#latex_slide do $[$body:slide_item
]*
) => do
    let binding <- liftTermElabM <| mkFreshUserName `slide
    let bindingStx := mkIdent binding
    let body <- liftTermElabM $ body.mapM elabItem
    let body := Syntax.TSepArray.ofElems body 
    let stx <- `(term| Slide.BasicSlide [] .none [$body,*])
    elabCommand <|
    (<- `(@[presentation]def $bindingStx:ident := $stx))
| `(#latex_slide [$opt,*] do $[$body:slide_item
]*
) => do
    let opts <- liftMacroM <| opt.getElems.mapM expandLatexOption
    let binding <- liftTermElabM <| mkFreshUserName `slide
    let bindingStx := mkIdent binding
    let body <- liftTermElabM $ body.mapM elabItem
    let body := Syntax.TSepArray.ofElems body 
    let stx <- `(term| Slide.BasicSlide [$opts,*] .none [$body,*])
    elabCommand <|
    (<- `(@[presentation]def $bindingStx:ident := $stx))

