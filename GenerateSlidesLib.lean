import Lean
import LeanTeX.SlideDSL
import LeanTeX.SlideRegistry
import LeanTeX.PackageRegistry
import LeanTeX.PreambleRegistry
import LeanTeX.LatexCommandRegistry
import LeanTeX.LatexGen

def beamerPreamble (packages: String) (preamble: String) : String :=
   s!"\\documentclass[aspectratio=169,xcolor=\{usenames,dvipsnames,svgnames}]\{beamer}\n\\usepackage\{tikz}\n{packages}\n{preamble}\n\\begin\{document}"
def beamerPostamble : String := "\\end{document}\n%%% Local Variables:\n%%% mode: LaTeX\n%%% TeX-master: t\n%%% End:\n"

structure LeanTeXConfig where
  slides: List Slide
  packages: String
  preamble: String

  latexCommand: Option String
  latexOptions: List String

  preambleTemplate: Option (forall packages preamble:String, String)


section Meta 
open Lean Elab Command Term Meta
syntax (name := getConfig) "#leantex_config" ident : command
@[command_elab getConfig]
unsafe def elabLeanTexConfig : CommandElab
| `(command| #leantex_config $name:ident) => do
   let slides <- liftTermElabM LeanTeX.loadSlidesStx
   let packages <- Syntax.mkStrLit <$> liftTermElabM LeanTeX.getPackageStr
   let preamble <- liftTermElabM LeanTeX.getPreambleStr
   let latexCommand <- liftTermElabM LeanTeX.getLaTeXCommand
   let latexOptions <- liftTermElabM LeanTeX.getLaTeXCommandOptions
   let preambleTemplate <- liftTermElabM LeanTeX.getPreambleTemplateCommand
   elabCommand $ <- `(command| def $name : LeanTeXConfig :=
       LeanTeXConfig.mk
          $slides
          $packages
          $preamble
          $latexCommand
          $latexOptions
          $preambleTemplate
   )
| _ => throwUnsupportedSyntax


end Meta
open Lean Meta

unsafe def generateSlides (config: LeanTeXConfig) : IO Unit := do
   let tex := config.slides.foldl (init := "") fun acc s => acc ++ renderSlide s
   let preambleTemplate := config.preambleTemplate.getD beamerPreamble
   let slides_tex := (preambleTemplate config.packages config.preamble ++ tex ++ beamerPostamble)
   let cmd := config.latexCommand.getD "pdflatex"
   let opts := config.latexOptions.toArray
   println! s!"{slides_tex}"
   if not $ <- ("build" : System.FilePath).pathExists then
      IO.FS.createDir "build"
   if <- ("static" : System.FilePath).pathExists then
       let _ <- IO.Process.run { cmd := "cp", args := #[ "-R", ".", "../build" ], cwd := "static" }
   IO.FS.writeFile "build/slides.tex" slides_tex
   let res <- IO.Process.spawn { cmd := cmd, args := opts ++ #["-shell-escape", "slides.tex"], cwd := "build"}
   let _ <- res.wait
   return ()
