import Lake
open Lake DSL

package LeanTeX where

lean_lib LeanTeX where

lean_lib GenerateSlidesLib where
  roots := #[`GenerateSlidesLib]

lean_exe GenerateSlides where
  root := `GenerateSlides

