import Lean
import LeanTeX.Slide
open Lean Elab Command Term Meta 

initialize preambleRegistry : SimplePersistentEnvExtension Name (Array Name) <-
   registerSimplePersistentEnvExtension {
      name := `preambleRegistry
      addEntryFn := (·.push ·)
      addImportedFn := fun bases => bases.foldl (· ++ ·) #[]
   }


def LeanTeX.addPreambleSnippet (name: Name) : MetaM Unit := do
   modifyEnv (preambleRegistry.addEntry · name)
   

def LeanTeX.getPreambleStr : MetaM (TSyntax `term) := do
   let env <- getEnv
   let decls := preambleRegistry.getState env |>.toList
   match decls with
   | [] => `("")
   | init :: decls =>
     let init <- `($(mkIdent init))
     decls.foldlM (init := init)
        (fun l r => `($l:term ++ "\n" ++ $(mkIdent r)))


