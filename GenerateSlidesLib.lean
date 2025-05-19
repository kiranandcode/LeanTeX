import Lean
import LeanTeX.SlideDSL
import LeanTeX.SlideRegistry
import LeanTeX.PackageRegistry
import LeanTeX.PreambleRegistry
import LeanTeX.LatexGen

def beamerPreamble (packages: String) (preamble: String) : String :=
   s!"\\documentclass[aspectratio=169,xcolor=\{usenames,dvipsnames,svgnames}]\{beamer}\n\\usepackage\{tikz}\n{packages}\n{preamble}\n\\begin\{document}"
def beamerPostamble : String := "\\end{document}\n%%% Local Variables:\n%%% mode: LaTeX\n%%% TeX-master: t\n%%% End:\n"

section Meta 
open Lean Elab Command Term Meta
syntax (name := getDeclaredSlides) "#declared_slides" ident : command

@[command_elab getDeclaredSlides]
unsafe def elabDeclaredSlides : CommandElab
| `(command| #declared_slides $name:ident) => do
   let slides <- liftTermElabM LeanTeX.loadSlidesStx
   elabCommand $ <- `(command| def $name : List Slide := $slides)
| _ => throwUnsupportedSyntax

syntax (name := getDeclaredPackages) "#declared_packages" ident : command

@[command_elab getDeclaredPackages]
unsafe def elabDeclaredPackages : CommandElab
| `(command| #declared_packages $name:ident) => do
   let packages <- liftTermElabM LeanTeX.getPackageStr
   let packages := Syntax.mkStrLit packages
   elabCommand $ <- `(command| def $name : String := $packages:str)
| _ => throwUnsupportedSyntax

syntax (name := getDeclaredPreamble) "#declared_preamble" ident : command

@[command_elab getDeclaredPreamble]
unsafe def elabDeclaredPreamble : CommandElab
| `(command| #declared_preamble $name:ident) => do
   let preamble <- liftTermElabM LeanTeX.getPreambleStr
   elabCommand $ <- `(command| def $name : String := $preamble:term)
| _ => throwUnsupportedSyntax


end Meta
open Lean Meta

unsafe def generateSlides (packages: String) (preamble: String) (allSlides: List Slide) : IO Unit := do
   let tex := allSlides.foldl (init := "") fun acc s => acc ++ renderSlide s
   let slides_tex := (beamerPreamble packages preamble ++ tex ++ beamerPostamble)
   println! s!"{slides_tex}"
   if not $ <- ("build" : System.FilePath).pathExists then
      IO.FS.createDir "build"
   if <- ("static" : System.FilePath).pathExists then
       let _ <- IO.Process.run { cmd := "cp", args := #[ "-R", ".", "../build" ], cwd := "static" }
   IO.FS.writeFile "build/slides.tex" slides_tex
   let _ <- IO.Process.spawn { cmd := "pdflatex", args := #["slides.tex"], cwd := "build"}
   return ()
