import Lean
import LeanTeX.Slide
open Lean Elab Command Term Meta

initialize preambleRegistry : SimplePersistentEnvExtension Name (Array Name) <-
   registerSimplePersistentEnvExtension {
      name := `preambleRegistry
      addEntryFn := (·.push ·)
      addImportedFn := fun bases => bases.foldl (· ++ ·) #[]
   }

initialize latexPreambleTemplateRegistry : SimplePersistentEnvExtension Name (Option Name) <-
   registerSimplePersistentEnvExtension {
      name := `preambleTemplateRegistry
      addEntryFn := (·.or ·)
      addImportedFn := fun bases => bases.foldl (fun acc v => v.foldl (·.getD ·) acc) Option.none
   }


def LeanTeX.addPreambleSnippet (name: Name) : MetaM Unit := do
   modifyEnv (preambleRegistry.addEntry · name)

def LeanTeX.setPreambleTemplateFunction (name: Name) : MetaM Unit := do
   if latexPreambleTemplateRegistry.getEntries (<- getEnv) |> (not ·.isEmpty) then
     logWarning "duplicate LaTeX preamble template declarations found."
   modifyEnv (latexPreambleTemplateRegistry.addEntry · name)

def LeanTeX.getPreambleStr : MetaM (TSyntax `term) := do
   let env <- getEnv
   let decls := preambleRegistry.getState env |>.toList
   match decls with
   | [] => `("")
   | init :: decls =>
     let init <- `($(mkIdent init))
     decls.foldlM (init := init)
        (fun l r => `($l:term ++ "\n" ++ $(mkIdent r)))

def LeanTeX.getPreambleTemplateCommand : MetaM (TSyntax `term) := do
   let env <- getEnv
   let decls := latexPreambleTemplateRegistry.getState env 
   match decls with
   | .none => `((Option.none : Option (String -> String -> String)))
   | .some cmd => `((Option.some $(mkIdent cmd) : Option (String -> String -> String)))

