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

def expandLatexOptionStr : TSyntax `latex_option -> MacroM String
| `(latex_option| $id:ident ) => do
   let id := id.getId.toString
   return id
| `(latex_option| $id:ident = $e:str ) => do
   let id := id.getId.toString
   return (id ++ "=" ++ e.getString)
| `(latex_option| $s:str ) => return s.getString
| _ => Macro.throwUnsupported


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
| `(latex_option| $s:str ) => return s
| _ => Macro.throwUnsupported

syntax ident : interval
syntax ident " - " (ident)? : interval
syntax "- " ident : interval
syntax "<" interval,+ ">" : interval_spec

def expandInterval : TSyntax `interval -> MacroM (TSyntax `term)
| `(interval| $id:ident) => `(LatexInterval.at $id)
| `(interval| $id:ident - ) => `(LatexInterval.after $id)
| `(interval| $st:ident - $ed:ident ) => `(LatexInterval.between $st $ed)
| `(interval| - $id:ident ) => `(LatexInterval.upto $id)
| _ => Macro.throwUnsupported

syntax " at " "(" term "," term ")" : tikz_at_spec
syntax " at " "(" term ")" : tikz_at_spec
syntax " at " ident : tikz_at_spec
syntax " at " str : tikz_at_spec

def expandTikzAtSpec : TSyntax `tikz_at_spec -> MacroM (TSyntax `term)
| `(tikz_at_spec| at ($t1:term , $t2:term)) => `(ToString.toString ($t1, $t2))
| `(tikz_at_spec| at ($t1:term)) => `(ToString.toString $t1)
| `(tikz_at_spec| at $id:ident) => `(ToString.toString $id)
| `(tikz_at_spec| at $id:str) => `($id)
| _ => Macro.throwUnsupported


declare_syntax_cat tikz_body
syntax "latex!"interpolatedLatexParser : tikz_body
syntax term ppLine : tikz_body

def expandTikzBody : TSyntax `tikz_body -> MacroM (TSyntax `term)
| `(tikz_body| latex!$str) => do
   let s <- expandInterpolatedLatex str (← `(String)) (← `(toString))
   return s
| `(tikz_body| $s:term) => `(term| ToString.toString $s)
| _ => Macro.throwUnsupported

syntax "node" (interval_spec)? ("[" latex_option,* "]")? (ident)? (tikz_at_spec)? tikz_body : tikz_item
syntax "draw" (interval_spec)? ("[" latex_option,* "]")? tikz_body : tikz_item

def expandIntervalSpec: TSyntax `interval_spec -> MacroM (TSyntax `term)
| `(interval_spec| < $it:interval,* > ) => do
  let elems <- it.getElems.mapM expandInterval
  `([$elems,*])
| _ => Macro.throwUnsupported

def elabTikzItem: TSyntax `tikz_item -> MacroM (TSyntax `term)
| `(tikz_item| draw $it:interval_spec [$opt:latex_option,*] $s:tikz_body) => do
   let it <- expandIntervalSpec it
   let opts <- opt.getElems.mapM expandLatexOption
   let s <- expandTikzBody s
   `(TikzCommand.draw $it [$opts,*] $s)
| `(tikz_item| draw [$opt:latex_option,*] $s:tikz_body) => do
   let opts <- opt.getElems.mapM expandLatexOption
   let s <- expandTikzBody s
   `(TikzCommand.draw [] [$opts,*] $s)
| `(tikz_item| draw $s:tikz_body) => do
   let s <- expandTikzBody s
   `(TikzCommand.draw [] [] $s)
| `(tikz_item| draw $it:interval_spec $s:tikz_body) => do
   let it <- expandIntervalSpec it
   let s <- expandTikzBody s
   `(TikzCommand.draw $it [] $s)

| `(tikz_item| node $it:interval_spec [$opt:latex_option,*] $e:tikz_at_spec $s:tikz_body) => do
   let it <- expandIntervalSpec it
   let opts <- opt.getElems.mapM expandLatexOption
   let s <- expandTikzBody s
   let e <- expandTikzAtSpec e
   `(TikzCommand.node $it [$opts,*] (Option.some $e) $s)
| `(tikz_item| node $it:interval_spec $e:tikz_at_spec $s:tikz_body) => do
   let it <- expandIntervalSpec it
   let s <- expandTikzBody s
   let e <- expandTikzAtSpec e
   `(TikzCommand.node $it [] (Option.some $e) $s)
| `(tikz_item| node $it:interval_spec [$opt:latex_option,*] $s:tikz_body) => do
   let it <- expandIntervalSpec it
   let opts <- opt.getElems.mapM expandLatexOption
   let s <- expandTikzBody s
   `(TikzCommand.node $it [$opts,*] (Option.none) $s)
| `(tikz_item| node $it:interval_spec $s:tikz_body) => do
   let it <- expandIntervalSpec it
   let s <- expandTikzBody s
   `(TikzCommand.node $it [] (Option.none) $s)
| `(tikz_item| node [$opt:latex_option,*] $e:tikz_at_spec $s:tikz_body) => do
   let opts <- opt.getElems.mapM expandLatexOption
   let s <- expandTikzBody s
   let e <- expandTikzAtSpec e
   `(TikzCommand.node [] [$opts,*] (Option.some $e) $s)
| `(tikz_item| node [$opt:latex_option,*] $s:tikz_body) => do
   let opts <- opt.getElems.mapM expandLatexOption
   let s <- expandTikzBody s
   `(TikzCommand.node [] [$opts,*] (Option.none) $s)
| `(tikz_item| node $e:tikz_at_spec $s:tikz_body) => do
   let s <- expandTikzBody s
   let e <- expandTikzAtSpec e
   `(TikzCommand.node [] [] (Option.some $e) $s)
| `(tikz_item| node $s:tikz_body) => do
   let s <- expandTikzBody s
   `(TikzCommand.node [] [] (Option.none) $s)
| _ => Macro.throwUnsupported


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
   let stx <- `(Slide.BasicSlide (.some $s) [$body,*])
   elabTerm stx .none
| `(slide do
$[
$body:slide_item
]*) => do
   let body <- body.mapM elabItem
   let body := Syntax.TSepArray.ofElems body 
   let stx <- `(Slide.BasicSlide .none [$body,*])
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
| _ => Macro.throwUnsupported

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
syntax "#latex_slide " (Parser.strLit)? " do " (colGt slide_item linebreak)* : command
syntax "#latex_command " interpolatedLatexParser : command

elab_rules : command
| `(#latex_command $str:interpolatedStr) => do
    let stx <-
      liftMacroM $
         expandInterpolatedLatex str (← `(String)) (← `(toString))
    let binding <- liftTermElabM <| mkFreshUserName `slide
    let bindingStx := mkIdent binding
    let stx <- `(term| Slide.RawSlide $stx)
    elabCommand <| (<- `(@[presentation]def $bindingStx:ident := $stx))

elab_rules : command
| `(#latex_slide_expr $expr:term) => do
    let binding <- liftTermElabM <| mkFreshUserName `slide
    let bindingStx := mkIdent binding
    elabCommand <| (<- `(@[presentation]def $bindingStx:ident := $expr))
| `(#latex_slide $t:str do $[$body:slide_item
]*) => do
    let binding <- liftTermElabM <| mkFreshUserName `slide
    let bindingStx := mkIdent binding
 
    let body <- liftTermElabM $ body.mapM elabItem
    let body := Syntax.TSepArray.ofElems body 
    let stx <- `(term| Slide.BasicSlide (.some $t) [$body,*])
    elabCommand <|
      (<- `(@[presentation]def $bindingStx:ident := $stx:term))

| `(#latex_slide do $[$body:slide_item
]*
) => do
    let binding <- liftTermElabM <| mkFreshUserName `slide
    let bindingStx := mkIdent binding
    let body <- liftTermElabM $ body.mapM elabItem
    let body := Syntax.TSepArray.ofElems body 
    let stx <- `(term| Slide.BasicSlide .none [$body,*])
    elabCommand <|
    (<- `(@[presentation]def $bindingStx:ident := $stx))

