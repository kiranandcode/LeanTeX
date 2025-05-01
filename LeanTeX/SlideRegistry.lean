import Lean
import LeanTeX.Slide
open Lean Elab Command Term Meta 

initialize slideRegistry : SimplePersistentEnvExtension Name (Array Name) <-
   registerSimplePersistentEnvExtension {
      name := `slideRegistry
      addEntryFn := fun arr decl => arr.push decl
      addImportedFn := fun bases => bases.foldl (· ++ ·) #[]
   }

initialize presentationAttr : TagAttribute <-
   registerTagAttribute `presentation "Marks a List Slide as a presentation"
      (validate := fun declName _stx => do
         let state : Core.State <- get
         set {state with env := slideRegistry.addEntry state.env declName}
         return ())

private def findPresentations : MetaM (List Name) := do
   let env <- getEnv
   let decls := slideRegistry.getState env
   return decls.toList

def LeanTeX.loadSlidesStx : TermElabM (TSyntax `term) := do
   let names : List Name <- findPresentations
   let names := names.map fun name => TSyntax.mk (mkIdent name)
   let names := names.toArray
   `(term| [$[$names:ident],*])

private unsafe def loadSlide (name: Name) : MetaM Slide := do
   let val <- evalConst Slide name
   return val

unsafe def LeanTeX.loadSlides : MetaM (List Slide) := do
    let slides <- findPresentations
    let slides <- slides.mapM loadSlide
    return slides
