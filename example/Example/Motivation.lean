import LeanTeX

namespace Motivation

@[presentation]
def slide_1 :=
   slide "Motivation" do
     latex![|
        \centering
        \begin{tikzpicture}[
           every node/.style={circle, draw=black, line width=0.5mm},
           every path/.style={line width=0.75mm}
        ]
          \node (n1) at (0,0) {2 langs?};
          \node (n2) at (0,-3) {1 lang!};
          \draw[>=stealth,dashed,->] (n1) -- (n2);
        \end{tikzpicture}
     |]


end Motivation
