import Lean
import LeanTeX.Slide
import LeanTeX.LatexLiteral
import LeanTeX.PackageRegistry
import LeanTeX.PreambleRegistry
open Lean Elab Command Term Meta

declare_syntax_cat slide_item
declare_syntax_cat slide_block

syntax (name := slide) "slide " (Parser.strLit)? " do " ppLine withPosition((colEq slide_item ppLine)+) : term

syntax 
   "\\begin" "{" ident "}" (colGt slide_item)*
   "\\end" "{" ident "}" : slide_item
syntax "\\item" "{" term  "}" : slide_item
syntax "latex!"interpolatedLatexParser : slide_item
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
   let stx <- `(Slide.mk (.some $s) [$body,*])
   elabTerm stx .none
| `(slide do
$[
$body:slide_item
]*) => do
   let body <- body.mapM elabItem
   let body := Syntax.TSepArray.ofElems body 
   let stx <- `(Slide.mk .none [$body,*])
   elabTerm stx .none


syntax (name := usepackage)
  "#usepackage" ident ("with " "options " "[" ident,* "] " )? : command

elab_rules : command
| `(#usepackage $name:ident with options [$opt:ident,*] ) => do
   let name := name.getId.toString
   let opts := opt.getElems.toList.map (·.getId.toString)
   liftTermElabM <| LeanTeX.addPackage name opts
| `(#usepackage $name:ident) => do
   let name := name.getId.toString
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
    let stx <- `(term| Slide.mk (.some $t) [$body,*])
    elabCommand <|
      (<- `(@[presentation]def $bindingStx:ident := $stx:term))

| `(#latex_slide do $[$body:slide_item
]*
) => do
    let binding <- liftTermElabM <| mkFreshUserName `slide
    let bindingStx := mkIdent binding
    let body <- liftTermElabM $ body.mapM elabItem
    let body := Syntax.TSepArray.ofElems body 
    let stx <- `(term| Slide.mk .none [$body,*])

    elabCommand <|
    (<- `(@[presentation]def $bindingStx:ident := $stx))

