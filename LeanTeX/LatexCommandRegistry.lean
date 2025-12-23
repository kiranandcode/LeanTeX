import Lean
import LeanTeX.Slide
open Lean Elab Command Term Meta

initialize latexCommandRegistry : SimplePersistentEnvExtension Name (Option Name) <-
   registerSimplePersistentEnvExtension {
      name := `latexCommandRegistry
      addEntryFn := (·.or ·)
      addImportedFn := fun bases => bases.foldl (fun acc v => v.foldl (·.getD ·) acc) Option.none
   }

initialize latexCommandOptionsRegistry : SimplePersistentEnvExtension Name (Array Name) <-
   registerSimplePersistentEnvExtension {
      name := `latexCommandOptionRegistry
      addEntryFn := (·.push ·)
      addImportedFn := fun bases => bases.foldl (· ++ ·) #[]
   }

def LeanTeX.setLatexCommand (name: Name) : MetaM Unit := do
   if latexCommandRegistry.getEntries (<- getEnv) |> (not ·.isEmpty) then
     logWarning "duplicate LaTeX command declaration found"
   modifyEnv (latexCommandRegistry.addEntry · name)

def LeanTeX.addLatexCommandOption (name: Name) : MetaM Unit := do
   modifyEnv (latexCommandOptionsRegistry.addEntry · name)

def LeanTeX.getLaTeXCommand : MetaM (TSyntax `term) := do
   let env <- getEnv
   let decls := latexCommandRegistry.getState env 
   match decls with
   | .none => `(Option.none)
   | .some cmd => `(Option.some $(mkIdent cmd))

def LeanTeX.getLaTeXCommandOptions : MetaM (TSyntax `term) := do
   let env <- getEnv
   let decls := latexCommandOptionsRegistry.getState env 
   match decls with
   | #[] => `(([] : List String))
   | options =>
      options.reverse.foldlM (init := <- `(term| ([] : List String)))
        (fun acc v =>
           `(term| $(mkIdent v) :: $acc))

