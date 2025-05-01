import LeanTeX

namespace Intro


@[presentation]
def slide_1 :=
   slide "Making Beamer Slides from Lean" do
     \begin{itemize}
       latex![| \itemsep3em |]
       \item{"Question: Why?"}
       \item{"Counter point: Why Not?"}
       \item{"Answer: Head Empty; Only Macros."}
     \end{itemize}

@[presentation]
def slide_temp :=
   slide "Enumerate example" do
      latex![|
         \begin{enumerate}
           \itemsep2em
           \item You can have enumerate
           \item You can have anything you want
           \item You can be happy!
         \end{enumerate}
      |]

end Intro
