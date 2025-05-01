import LeanTeX.Slide

def renderTikzPictureContext (t: TikzPictureContext) : String :=
  ",".intercalate <|
    t.misc ++
    t.styles.toList.map (fun (k,v) => s!"{k}/.style=\{{v}}")

def renderLatexInterval : LatexInterval -> String
| .at n => s!"{n}"
| .between st ed => s!"{st}-{ed}"
| .after st => s!"{st}-"
| .upto ed => s!"-{ed}"

def renderLatexIntervals (intervals: List LatexInterval) :=
  ",".intercalate <| intervals.map renderLatexInterval

def renderTikzCommand : TikzCommand -> String
| .text t => t
| .node range styles pos body =>
  let rangeStr :=
    if range.isEmpty then ""
    else renderLatexIntervals range |> (s!"<{·}>")
  let stylesStr :=
     if styles.isEmpty then ""
     else ",".intercalate styles |> (s!"[{·}]")
  let posStr := if let .some pos := pos then s!" at {pos}" else ""
  s!"\\node{rangeStr}{stylesStr}{posStr} \{{body}};"
| .draw range styles body =>
  let rangeStr :=
    if range.isEmpty then ""
    else renderLatexIntervals range |> (s!"<{·}>")
  let stylesStr :=
     if styles.isEmpty then ""
     else ",".intercalate styles |> (s!"[{·}]")
  s!"\\draw{rangeStr}{stylesStr} {body};"



def renderContent : List SlideContent -> String
| [] => ""
| SlideContent.item txt :: rest =>
   s!"\\item {txt}\n" ++ renderContent rest
| SlideContent.environment tag contents :: rest =>
   s!"\\begin\{{tag}}\n"
   ++ renderContent contents
   ++ s!"\\end\{{tag}}\n"
   ++ renderContent rest
| SlideContent.text txt :: rest =>
   s!"{txt}\n" ++ renderContent rest
| SlideContent.block elts :: rest =>
   "%start_block\n" ++ renderContent elts
   ++ "\n%end_block\n" ++ renderContent rest
| SlideContent.tikzpicture ctx elts :: rest =>
  s!"\\begin\{tikzpicture}[{renderTikzPictureContext ctx}]\n"
  ++ "\n".intercalate (elts.map renderTikzCommand)
  ++ "\n\\end{tikzpicture}\n"
  ++ renderContent rest

def renderSlide (s: Slide) : String :=
  let title := match s.title with
    | .none => ""
    | .some title => s!"\{{title}}"
  s!"\\begin\{frame}{title}\n{renderContent s.content}\n\\end\{frame}"
