(TeX-add-style-hook "getting_started"
 (lambda ()
    (LaTeX-add-labels
     "/the/environment-the-environment"
     "sec-1"
     "sec-2"
     "sec-3"
     "sec-4"
     "sec-5")
    (TeX-run-style-hooks
     "hyperref"
     "amssymb"
     "wasysym"
     "marvosym"
     "textcomp"
     "amsmath"
     "ulem"
     "normalem"
     "rotating"
     "wrapfig"
     "float"
     "longtable"
     "graphicx"
     "fixltx2e"
     ""
     "fontenc"
     "T1"
     "inputenc"
     "utf8"
     "latex2e"
     "art11"
     "article"
     "11pt")))

