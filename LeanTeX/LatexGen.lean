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
| .pathLetNode bindings range styles pos name body =>
   let bindingsStr := ",".intercalate $ bindings.zipIdx.map fun (elt, idx) => s!"\\p{idx+1} = {elt}"
  let rangeStr :=
    if range.isEmpty then ""
    else renderLatexIntervals range |> (s!"<{·}>")
  let stylesStr :=
     if styles.isEmpty then ""
     else ",".intercalate styles |> (s!"[{·}]")
  let posStr := if let .some pos := pos then s!" at {pos}" else ""
  let nameStr := if let .some name := name then s!" ({name}) " else ""
  s!"\\path let {bindingsStr} in node{rangeStr}{stylesStr}{nameStr}{posStr} \{{body}};"
| .text t => t
| .node range styles pos name body =>
  let rangeStr :=
    if range.isEmpty then ""
    else renderLatexIntervals range |> (s!"<{·}>")
  let stylesStr :=
     if styles.isEmpty then ""
     else ",".intercalate styles |> (s!"[{·}]")
  let posStr := if let .some pos := pos then s!" at {pos}" else ""
  let nameStr := if let .some name := name then s!" ({name}) " else ""
  s!"\\node{rangeStr}{stylesStr}{nameStr}{posStr} \{{body}};"
| .draw range styles body =>
  let rangeStr :=
    if range.isEmpty then ""
    else renderLatexIntervals range |> (s!"<{·}>")
  let stylesStr :=
     if styles.isEmpty then ""
     else ",".intercalate styles |> (s!"[{·}]")
  s!"\\draw{rangeStr}{stylesStr} {body};"
| .layer layer body =>
  s!"\\begin\{pgfonlayer}\{{layer}}\n" ++ 
  "\n".intercalate (body.map renderTikzCommand) ++
  s!"\n\\end\{pgfonlayer}\n"
| .block body =>
  "\n".intercalate (body.map renderTikzCommand)

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
  match s with
  | .BasicSlide opts title content =>
     let opts  := match opts with
       | [] => ""
       | opts => "[" ++ ", ".intercalate opts ++ "]"
     let title := match title with
       | .none => ""
       | .some title => s!"\{{title}}"
     s!"\\begin\{frame}{opts}{title}\n{renderContent content}\n\\end\{frame}\n"
  | .RawSlide s => s
