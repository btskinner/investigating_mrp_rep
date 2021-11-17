# Documents

The LaTeX file to make produce a PDF of the paper's tables and figures
is in this directory. After running all analyses and creating tables
(`*.tex`) and figures (`*.pdf`), you can compile the PDF document
using

``` shell
latexmk -pdf -silent tables_figures.tex
```

from the command line or most LaTeX GUI applications.
