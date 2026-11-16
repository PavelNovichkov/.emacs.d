;; -*-coding: utf-8;-*-

;;; Common LaTeX tables.

(define-abbrev-table 'my/latex-math-abbrev-table
  (append
   '(("acos" "\\arccos")
     ("ast" "\\ast")
     ("asin" "\\arcsin")
     ("atan" "\\arctan")
     ("al" "\\alpha")
     ("ap" "\\approx")
     ("ar" "\\rightarrow")
     ("arb" "\\leftrightarrow") ; "arrow both"
     ("arl" "\\leftarrow")
     ("aru" "\\uparrow")
     ("ard" "\\downarrow")
     ("ba" "\\mathbb{A}")
     ("bc" "\\mathbb{C}")
     ("be" "\\beta")
     ("bh" "\\mathbb{H}")
     ("bk" "\\mathbb{k}")
     ("bn" "\\mathbb{N}")
     ("bq" "\\mathbb{Q}")
     ("br" "\\mathbb{R}")
     ("bz" "\\mathbb{Z}")
     ("ca" "\\cap")
     ("cd" "\\cdot")
     ("cds" "\\cdots")
     ("ci" "\\circ")
     ("ch" "\\chi")
     ("col" "\\colon")
     ("cos" "\\cos")
     ("cu" "\\cup")
     ("da" "\\dagger")
     ("de" "\\delta")
     ("do" "\\dots")
     ("el" "\\ell")
     ("em" "\\emptyset")
     ("ep" "\\epsilon")
     ("eq" "\\equiv")
     ("et" "\\eta")
     ("ex" "\\exists")
     ("exp" "\\exp")
     ("fa" "\\forall")
     ("fl" "\\flat")
     ("ga" "\\gamma")
     ("ge" "\\geq")
     ("gg" "\\gg")
     ("gs" "\\gtrsim")
     ("hb" "\\hbar")
     ("im" "\\Im")
     ("imp" "\\implies")
     ("in" "\\in")
     ("inf" "\\infty")
     ("int" "\\int")
     ("io" "\\iota")
     ("ka" "\\kappa")
     ("la" "\\lambda")
     ("ld" "\\ldots")
     ("le" "\\leq")
     ("ll" "\\ll")
     ("log" "\\log")
     ("loar" "\\longrightarrow")
     ("loarb" "\\longleftrightarrow") ; "long arrow both"
     ("loarl" "\\longleftarrow")
     ("lomt" "\\longmapsto")
     ("ls" "\\lesssim")
     ("lt" "\\ltimes")
     ("max" "\\max")
     ("mid" "\\mid")
     ("min" "\\min")
     ("mi" "\\imath")
     ("mj" "\\jmath")
     ("mp" "\\mp")
     ("mt" "\\mapsto")
     ("mu" "\\mu")
     ("na" "\\nabla")
     ("ne" "\\neq")
     ("nex" "\\nexists")
     ("ni" "\\ni")
     ("nin" "\\notin")
     ("nm" "\\nmid")
     ("no" "\\not")
     ("nu" "\\nu")
     ("om" "\\omega")
     ("op" "\\oplus")
     ("ot" "\\otimes")
     ("pa" "\\parallel")
     ("pd" "\\partial") ; "partial derivative"
     ("pe" "\\perp")
     ("ph" "\\phi")
     ("pi" "\\pi")
     ("pm" "\\pm")
     ("pr" "\\prod")
     ("pri" "\\prime")
     ("ps" "\\psi")
     ("pt" "\\propto")
     ("re" "\\Re")
     ("rh" "\\rho")
     ("rl" "\\langle") ; "r" for "angle" as in delimiter templates
     ("rr" "\\rangle")
     ("rt" "\\rtimes")
     ("th" "\\theta")
     ("sb" "\\subset")
     ("sbe" "\\subseteq")
     ("sbn" "\\subsetneq")
     ("se" "\\simeq")
     ("si" "\\sigma")
     ("sim" "\\sim")
     ("sin" "\\sin")
     ("sh" "\\sharp")
     ("sm" "\\setminus")
     ("sp" "\\supset")
     ("su" "\\sum")
     ("ta" "\\tau")
     ("tan" "\\tan")
     ("ti" "\\times")
     ("to" "\\to")
     ("up" "\\upsilon")
     ("ve" "\\varepsilon")
     ("vk" "\\varkappa")
     ("vp" "\\varphi")
     ("vpi" "\\varpi")
     ("vr" "\\varrho")
     ("vs" "\\varsigma")
     ("vt" "\\vartheta")
     ("we" "\\wedge")
     ("xi" "\\xi")
     ("ze" "\\zeta"))
   (my/abbrev-templates
    '(("bar" "\\bar{" p "}" q)
      ("bb" "\\mathbb{" p "}" q)
      ("bf" "\\mathbf{" p "}" q)
      ("bm" "\\bm{" p "}" q)
      ("cal" "\\mathcal{" p "}" q)
      ("cfr" "\\cfrac{" p "}{" p "}" q)
      ("dfr" "\\dfrac{" p "}{" p "}" q)
      ; delimeter definitions matching those of evil-tex
      ("dC" "\\{ " p " \\}" q)
      ("dN" "\\lVert " p " \\rVert" q)
      ("dP" "( " p " )" q)
      ("dR" "\\langle " p " \\rangle" q)
      ("dS" "[ " p " ]" q)
      ("dV" "\\lvert " p " \\rvert" q)
      ("dc" "\\left\\{ " p " \\right\\}" q)
      ("dn" "\\left\\lVert " p " \\right\\rVert" q)
      ("dp" "\\left( " p " \\right)" q)
      ("dr" "\\left\\langle " p " \\right\\rangle" q)
      ("ds" "\\left[ " p " \\right]" q)
      ("dv" "\\left\\lvert " p " \\right\\rvert" q)
      ("fk" "\\mathfrak{" p "}" q)
      ("fr" "\\frac{" p "}{" p "}" q)
      ("hat" "\\hat{" p "}" q)
      ("it" "\\mathit{" p "}" q)
      ("rm" "\\mathrm{" p "}" q)
      ("scr" "\\mathscr{" p "}" q)
      ("sl" "\\slashed{" p "}" q)
      ("sq" "\\sqrt{" p "}" q)
      ("te" "\\text{" p "}" q)
      ("til" "\\tilde{" p "}" q)
      ("vec" "\\vec{" p "}" q)
      ("xl" "\\xleftarrow{" p "}" q)
      ("xr" "\\xrightarrow{" p "}" q))))
  :enable-function #'texmathp)

(define-abbrev-table 'my/latex-not-math-abbrev-table
  (my/abbrev-templates
   '(("m" "\\(" p "\\)" q)
     ("ali" & "\\begin{align}" n> p n "\\end{align}" > n> q)
     ("equ" & "\\begin{equation}" n> p n "\\end{equation}" > n> q)))
  :enable-function (lambda () (not (texmathp))))

(define-abbrev-table 'my/latex-org-abbrev-table
  (my/abbrev-templates
   '(("env" & "\\begin{" (s env) "}" n> p n "\\end{" (s env) "}" > n> q)))
  :parents (list my/latex-math-abbrev-table my/latex-not-math-abbrev-table))

;;; Major mode tables.

(define-abbrev-table 'LaTeX-mode-abbrev-table
  (my/abbrev-templates
   '(("sec" "\\section{" p "}" n q))))

;; AUCTeX sets :parents so we have to add ours after it loads.
(with-eval-after-load 'latex
  (abbrev-table-put
   LaTeX-mode-abbrev-table
   :parents
   (cons my/latex-org-abbrev-table
         (abbrev-table-get LaTeX-mode-abbrev-table :parents))))

(define-abbrev-table 'org-mode-abbrev-table
  (my/abbrev-templates
   '(("src" & "#+begin_src " p n> p n> "#+end_src" n> q)
     ("quo" & "#+begin_quote" n> p n> "#+end_quote" n> q))))

;; Org sets :parents so we have to add ours after it loads.
(with-eval-after-load 'org
  (abbrev-table-put
   org-mode-abbrev-table
   :parents
   (cons my/latex-org-abbrev-table
         (abbrev-table-get org-mode-abbrev-table :parents))))

(define-abbrev-table 'text-mode-abbrev-table
  '(("afaik" "as far as I know")
    ("btw" "by the way")
    ("il" "I’ll")
    ("im" "I’m")
    ("iv" "I’ve")))

(define-abbrev-table 'julia-mode-abbrev-table
  (my/abbrev-templates
   '(
     ("doc" "\"\"\"" n p n "\"\"\"" n q)
     ("if" "if " p n> p n "end" > q)
     ("fn" "function " p "(" p ")" n> p n "end" > q)
     ("for" "for " p " in " p n> p n "end" > q)
     ("mod" "module " p n n q n n "end")
     ("mstr" "mutable struct " p n> p n "end" > q)
     ("str" "struct " p n> p n "end" > q)
     ("while" "while " p n> p n "end" > q))))
