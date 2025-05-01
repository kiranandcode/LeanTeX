import Lean
open Lean Meta

structure TikzPictureContext where
  styles : Std.HashMap String String
  misc : List String
deriving Repr

inductive LatexInterval where
| at (n: Nat)
| between (st ed: Nat)
| after (st: Nat)
| upto (ed: Nat)
deriving Repr

inductive TikzCommand where
| text (t: String)
| node (range: List LatexInterval) (styles: List String) (pos: Option String) (body: String)
| draw (range: List LatexInterval) (styles: List String) (body: String)
deriving Repr

inductive SlideContent
| item (s: String)
| environment (s: String) (elts: List SlideContent)
| text (t: String)
| block (elts: List SlideContent)
| tikzpicture (context: TikzPictureContext) (elts: List TikzCommand)
deriving Repr


structure Slide where
  title: Option String
  content: List SlideContent
deriving Repr

