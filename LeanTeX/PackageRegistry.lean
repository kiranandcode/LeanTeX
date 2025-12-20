import Lean
import LeanTeX.Slide
open Lean Elab Command Term Meta

structure UsePackageDecl where
  name: String
  options: List String
deriving Repr, Inhabited

instance : ToString UsePackageDecl where
  toString decl :=
     if decl.options.isEmpty
     then s!"\\usepackage\{{decl.name}}"
     else
       let optionsStr := ",".intercalate  decl.options
       s!"\\usepackage[{optionsStr}]\{{decl.name}}"

initialize packageRegistry : SimplePersistentEnvExtension UsePackageDecl (PersistentHashMap String UsePackageDecl) <-
   let addEntry := fun map decl => map.insert decl.name decl
   registerSimplePersistentEnvExtension {
      name := `packageRegistry
      addEntryFn := addEntry
      addImportedFn :=
       fun bases => bases.foldl (init := default)
        (fun acc vl => vl.foldl addEntry acc)
   }


def LeanTeX.addPackage (name: String) (options: List String) : MetaM Unit := do
   let env <- getEnv
   let decls := packageRegistry.getState env
   match decls.find? name with
   | .none =>
      setEnv <| packageRegistry.addEntry env ⟨name, options⟩
   | .some package =>
      if package.options != options then
        throwError s!"redeclaration of package {name} with different options {package.options} != {options}"

def LeanTeX.getPackageStr : MetaM String := do
   let env <- getEnv
   let decls := packageRegistry.getState env
   return "\n".intercalate <| decls.toList.map (·.2 |> toString)
