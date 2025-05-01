import Lean

def fontsize (txt: String) (size : Int := 20) :=
  s!"\\fontsize\{{size}}\{{size}}\\selectfont\{{txt}}"

