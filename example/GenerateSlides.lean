
import Example
import GenerateSlidesLib

#declared_slides allSlides
#declared_packages allPackages
#declared_preamble allPreamble

unsafe def main : IO Unit := do
   generateSlides allPackages allPreamble allSlides
