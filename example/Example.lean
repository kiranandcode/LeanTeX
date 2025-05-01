import LeanTeX
import Example.Utils
#usepackage beamerthemeleantex

#latex_preamble [|
\graphicspath{{./images/}}
\title{\vspace{-2em}\\ [|
   fontsize (size := 30) "LeanTeX: Latex presentations in Lean" 
|]}
\author{[|fontsize (size:=18) "Kiran (She/Her)" |]}
\institute{[|fontsize (size:=15) "UIUC"|]}
\date{[|fontsize (size:=15) "April 2024"|]}
|]

#latex_slide do
     latex![|
       \titlepage
     |]

#latex_slide do
  with steps [step1, step2, step3, step4] do
     latex![|
       \begin{tikzpicture}
          \draw<@{step1}->  (0,0) rectangle ++(1,1);
          \draw<@{step2}->  (1,0) rectangle ++(1,1);
          \draw<@{step3}->  (2,0) rectangle ++(1,1);
          \draw<@{step4}->  (3,0) rectangle ++(1,1);
       \end{tikzpicture}
     |]
  SlideContent.text "%example"  
