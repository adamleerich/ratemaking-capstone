
Rscript ".\R\create-data.R"


"C:/Program Files/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS README.md --to latex --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash --output .\share\Instructions.pdf --template "C:\PROGRA~1\R\R-35~1.1\library\RMARKD~1\rmd\latex\DEFAUL~3.TEX" --highlight-style tango --latex-engine pdflatex --variable graphics=yes --variable "geometry:margin=1in" --variable "compact-title:yes" 


copy /Y .\data\states.csv .\share\
copy /Y .\R\resources.R .\share\


7z a ratemaking-capstone.zip .\share\*


copy /Y .\share\* ..\capstone-project\


