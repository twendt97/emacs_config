

(defvar mupad-all-completions
  '(
;; all incl libraries
"Above"           "Adaptive"           "All"                "Alldata"
"Always"          "Any"                "Append"             "Approx"
"Arrows"          "Assumptions"        "Attached"           "Ax"
"Axes"            "AxesScaling"        "Axiom"              "AxiomConstructor"
"BUTCHER6"        "BackGround"         "BackSubstitution"   "Banded"
"Beam"            "Bin"                "Binary"             "Bottom"
"Box"             "CK45"               "CK54"               "CameraPoint"
"Capacity"        "Cat"                "Category"           "CategoryConstructor"
"Center"          "Ci"                 "Closed"             "Coeffs"
"Color"           "ColorPatches"       "Colors"             "Column"
"Complete"        "Concave"            "Constrained"        "Contours"
"Convex"          "Corner"             "Curve"              "Curves"
"Cyclic"          "D"                  "DIGITS"             "DOM_ARRAY"
"DOM_BOOL"        "DOM_COMPLEX"        "DOM_DOMAIN"         "DOM_EXEC"
"DOM_EXPR"        "DOM_FAIL"           "DOM_FLOAT"          "DOM_FUNC_ENV"
"DOM_IDENT"       "DOM_INT"            "DOM_LIST"           "DOM_NIL"
"DOM_NULL"        "DOM_POINT"          "DOM_POLY"           "DOM_POLYGON"
"DOM_PROC"        "DOM_PROC_ENV"       "DOM_RAT"            "DOM_SET"
"DOM_STRING"      "DOM_TABLE"          "DOM_VAR"            "DOPRI45"
"DOPRI54"         "DashedLines"        "DebugOnTheFly"      "DegInvLexOrder"
"DegLex"          "DegRevLex"          "Degree"             "DegreeOrder"
"Delete"          "Diagonal"           "Diagonalization"    "Dimension"
"Dom"             "DomainConstructor"  "DomainsOnly"        "DualPrices"
"E"               "EMPTY"              "EULER"              "EULER1"
"Error"           "Eweight"            "Exact"              "Expr"
"Extended"        "FAIL"               "FALSE"              "Factor"
"Filled"          "FilledCircles"      "First"              "Flat"
"FloatFormat"     "FocalPoint"         "FontSize"           "ForeGround"
"Format"          "Frames"             "Frobenius"          "GC"
"GL"              "GT"                 "Gauss"              "GaussChebyshev"
"GaussLegendre"   "GaussTschebyscheff" "Gif"                "Grid"
"HISTORY"         "Height"             "HiddenLine"         "I"
"Im"              "Include"            "Independent"        "Indets"
"Index"           "IndexList"          "Infinity"           "IntMod"
"Interactive"     "Interpolation"      "Intersect"          "Krylov"
"LEVEL"           "LIB_PATH"           "Labeling"           "Labels"
"Laurent"         "Left"               "Length"             "LexOrder"
"LieGroupAction"  "Line"               "LineStyle"          "LineWidth"
"List"            "Log"                "Logic"              "MAXDEPTH"
"MAXLEVEL"        "MSDOS"              "MaxDegree"          "Mesh"
"MinorExpansion"  "Mode"               "ModuleTrace"        "Multiple"
"NC"              "NIL"                "NO_CONVERGENCE"     "NO_SOLUTION"
"Name"            "Named"              "Nary"               "Natural"
"Network"         "New"                "NewtonCotes"        "NoCheck"
"NoErrors"        "NoLeftVectors"      "NoRightVectors"     "NonNegative"
"NonNested"       "None"               "NotAKnot"           "O"
"OPTIMAL"         "ORDER"              "Only"               "Open"
"Origin"          "PI"                 "PRETTY_PRINT"       "Path"
"Periodic"        "Piechart"           "Piechart3d"         "Plain"
"PlotDevice"      "PointStyle"         "PointWidth"         "PolyExpr"
"PostMap"         "Postfix"            "PreMap"             "Pref"
"Prefix"          "Pretty"             "PrincipalValue"     "Proc"
"Puiseux"         "Quiet"              "Quo"                "READ_PATH"
"RGB"             "RK4"                "RKF34"              "RKF43"
"RKF45a"          "RKF45b"             "RKF54a"             "RKF54b"
"RKF78"           "RKF87"              "ROUNDING"           "Raster"
"RatExpr"         "Re"                 "Recursive"          "RelativeError"
"Rem"             "Reorder"            "Right"              "Root"
"RootOf"          "SEED"               "Sample"             "Scales"
"Scaling"         "Sceneoptions"       "Series"             "Si"
"Smoothness"      "Special"            "Stepsize"           "Style"
"Sum"             "Surface"            "Symbolic"           "SymbolicAnalysis"
"System"          "TEST_PATH"          "TEXTWIDTH"          "TRUE"
"Taylor"          "TaylorExpansion"    "Term"               "Terms"
"Test"            "Text"               "Ticks"              "Title"
"TitlePosition"   "Titles"             "ToInfinity"         "ToMinusInfinity"
"ToNearest"       "ToZero"             "TrailingZeroes"     "Transposed"
"Type"            "UNBOUNDED"          "UNKNOWN"            "UnConstrained"
"Unique"          "Unquoted"           "Unrestricted"       "Unsimplified"
"Up"              "UsePrimeTab"        "User"               "Values"
"VectorOf"        "ViewingBox"         "Vweight"            "WRITE_PATH"
"Warning"         "Write"              "X11"                "YRange"
"_act_proc_env"   "_and"               "_assign"            "_break"
"_case"           "_concat"            "_constructor"       "_div"
"_divide"         "_equal"             "_eval_entry"        "_exprseq"
"_fconcat"        "_fnest"             "_for"               "_for_down"
"_for_in"         "_if"                "_index"             "_intersect"
"_invert"         "_leequal"           "_less"              "_minus"
"_mod"            "_mult"              "_negate"            "_next"
"_not"            "_or"                "_parser_callback"   "_parser_config"
"_pattern"        "_plus"              "_power"             "_pref"
"_print_userinfo" "_procdef"           "_quit"              "_range"
"_repeat"         "_seqgen"            "_seqin"             "_stmtseq"
"_subtract"       "_unequal"           "_union"             "_userinfo_level"
"_while"          "abs"                "ac_match"           "adt"
"alias"           "anames"             "and "               "append"
"arccos"          "arccosh"            "arccot"             "arccoth"
"arccsc"          "arccsch"            "arcsec"             "arcsech"
"arcsin"          "arcsinh"            "arctan"             "arctanh"
"args"            "array"              "assign"             "assign_elems"
"assume"          "begin"              "bernoulli"          "besselJ"
"besselK"         "besselY"            "beta"               "binomial"
"bool"            "break"              "built_in"           "bytes"
"case "           "catalan"            "ceil"               "client_call"
"client_id"       "coeff"              "collect"            "combinat"
"combine"         "complexInfinity"    "conjugate"          "contains"
"content"         "context"            "cos"                "cosh"
"cot"             "coth"               "csc"                "csch"
"debug"           "degree"             "degreevec"          "denom"
"detools"         "diff"               "dilog"              "dirac"
"discont"         "div "               "divide"             "do "
"domainfunccall"  "domains"            "domattr"            "domtype"
"downto "         "eint"               "elif "              "else "
"end"             "end_case"           "end_for"            "end_if"
"end_proc"        "end_repeat"         "end_while"          "erf"
"erfc"            "error"              "eval"               "evalassign"
"evalp"           "exp"                "expand"             "export"
"expose"          "expr"               "expr2text"          "external"
"extnops"         "extop"              "extsubsop"          "faclib"
"fact"            "factor"             "factors"            "fclose"
"finput"          "float"              "float_gen"          "floor"
"fopen"           "for "               "fp"                 "fprint"
"frac"            "fread"              "freeze"             "from "
"ftextinput"      "fun"                "func"               "func_env"
"funcattr"        "gamma"              "gcd"                "gcdex"
"gcdlib"          "gcov"               "generate"           "genident"
"genpoly"         "getpid"             "getprop"            "gprof"
"groebner"        "has"                "hastype"            "heaviside"
"help"            "history"            "hold"               "icontent"
"id"              "if "                "ifactor"            "igamma"
"igcd"            "igcdex"             "ilcm"               "import"
"in "             "indets"             "index_val"          "infinity"
"info"            "input"              "int"                "int2text"
"intersect "      "intlib"             "iquo"               "irem"
"irreducible"     "is"                 "isPositiveDefinite" "isprime"
"isqrt"           "iszero"             "ithprime"           "lambertv"
"lambertw"        "last"               "lasterror"          "lcm"
"lcoeff"          "ldegree"            "length"             "level"
"limit"           "linalg"             "linopt"             "linsolve"
"listlib"         "lllint"             "lmonomial"          "ln"
"load"            "loadlib"            "loadmod"            "loadproc"
"local "          "lterm"              "map"                "mapcoeffs"
"maprat"          "match"              "matchlib"           "max"
"min"             "minus "             "misc"               "mod "
"modp"            "mods"               "module"             "multcoeffs"
"name"            "new"                "new_bytecode"       "new_domain"
"newfuncarg"      "newpurefunc"        "next"               "nextprime"
"nohistory"       "nops"               "norm"               "normal"
"not "            "nterms"             "nthcoeff"           "nthmonomial"
"nthterm"         "null"               "numer"              "numeric"
"numlib"          "ode"                "of "                "op"
"op_fconcat"      "op_plusmult"        "op_power"           "operator"
"option "         "or "                "orthpoly"           "otherwise"
"output"          "output_format"      "output_type"        "package"
"partfrac"        "patchlevel"         "pathname"           "pdivide"
"phi"             "piecewise"          "plot"               "plot2d"
"plot3d"          "plotfunc"           "plotlib"            "point"
"poly"            "poly2list"          "polygon"            "polylib"
"polylog"         "powermod"           "print"              "proc"
"product"         "prog"               "property"           "protect"
"protected"       "protocol"           "psi"                "ptime"
"quit"            "radsimp"            "random"             "rationalize"
"rd_abs"          "rd_divide"          "rd_float2text"      "rd_getsign"
"rd_int2float"    "rd_isfinite"        "rd_mult"            "rd_negate"
"rd_plus"         "rd_round"           "rd_subtract"        "rd_text2float"
"read"            "rec"                "rectform"           "register"
"repeat "         "reset"              "return"             "revert"
"rewrite"         "round"              "rtime"              "sec"
"sech"            "select"             "series"             "setuserinfo"
"sign"            "simplify"           "sin"                "sincos"
"sinh"            "sinhcosh"           "slot"               "solve"
"solvelib"        "sort"               "specfunc"           "split"
"sqrt"            "stats"              "stdlib"             "step"
"stringlib"       "strmatch"           "student"            "subs"
"subsex"          "subsop"             "substring"          "sum"
"sysassign"       "sysevalassign"      "sysname"            "sysorder"
"system"          "sysunassign"        "table"              "tan"
"tanh"            "taylor"             "tbl2text"           "tcoeff"
"testargs"        "testtype"           "text2expr"          "text2int"
"text2list"       "text2tbl"           "textinput"          "then"
"time"            "to"                 "toBeDefined"        "transform"
"traperror"       "trunc"              "type"               "unalias"
"unassign"        "unassume"           "undefined"          "unexport"
"unfreeze"        "union "             "universe"           "unloadlib"
"unloadmod"       "unprotect"          "until"              "userinfo"
"val"             "version"            "warning"            "while "
"write"           "xCK45"              "xCK54"              "xDOPRI45"
"xDOPRI54"        "xRKF34"             "xRKF43"             "xRKF45a"
"xRKF45b"         "xRKF54a"            "xRKF54b"            "xRKF78"
"xRKF87"          "zeta"               "zip"
))

(message "loadling mupad-all done")

(defvar mupad-libraries-list
  ;; all libraries
  '(
"Ax"                  "Axiom"             "AxiomConstructor" "Cat"         "Category"
"CategoryConstructor" "DOM_ARRAY"         "DOM_BOOL"         "DOM_COMPLEX" "DOM_DOMAIN"
"DOM_EXEC"            "DOM_EXPR"          "DOM_FAIL"         "DOM_FLOAT"   "DOM_FUNC_ENV"
"DOM_IDENT"           "DOM_INT"           "DOM_LIST"         "DOM_NIL"     "DOM_NIL"
"DOM_NULL"            "DOM_POINT"         "DOM_POLY"         "DOM_POLYGON" "DOM_PROC"
"DOM_RAT"             "DOM_SET"           "DOM_STRING"       "DOM_TABLE"   "DOM_VAR"
"Dom"                 "DomainConstructor" "Network"          "O"           "Pref"
"RGB"                 "Series"            "Type"             "adt"         "combinat"
"detools"             "domains"           "faclib"           "fp"          "gcdlib"
"generate"            "groebner"          "import"           "intlib"      "linalg"
"linopt"              "listlib"           "matchlib"         "misc"        "module"
"numeric"             "numlib"            "ode"              "orthpoly"    "output"
"piecewise"           "plotlib"           "polylib"          "prog"        "property"
"rec"                 "rectform"          "solvelib"         "specfunc"    "stats"
"stdlib"              "stringlib"         "student"          "transform"
))

(message "loadling mupad-libs done")

(defvar mupad-libraries-completion-alist
;; all libraries with methods
'(
("Ax" .
(
"canonicalOrder" "canonicalRep" "canonicalUnitNormal" "closedUnitNormals" "effectiveOperation"
"noZeroDivisors" "normalRep"    "systemRep"
))

("Cat" .
(
"AbelianGroup"                "AbelianMonoid"             "AbelianSemiGroup"
"Algebra"                     "CancellationAbelianMonoid" "CommutativeRing"
"DifferentialFunctionCat"     "DifferentialRing"          "DifferentialVariableCat"
"EntireRing"                  "EuclideanDomain"           "FactorialDomain"
"Field"                       "FiniteCollectionCat"       "FiniteMonoidRing"
"GcdDomain"                   "Group"                     "HomogeneousFiniteCollectionCat"
"HomogeneousFiniteProductCat" "IntegralDomain"            "LeftModule"
"MatrixCat"                   "Module"                    "Monoid"
"MonoidRing"                  "OrderedAbelianMonoid"      "OrderedMonoid"
"OrderedSet"                  "PartialDifferentialRing"   "PolynomialCat"
"PrincipalIdealDomain"        "QuotientField"             "RestrictedDifferentialFunctionCat"
"RightModule"                 "Ring"                      "Rng"
"SemiGroup"                   "Set"                       "SetCat"
"SkewField"                   "SquareMatrixCat"           "UnivariatePolynomialCat"
"UnivariateSkewPolynomialCat" "VectorSpace"
))

("Dom" .
(
"AlgebraicExtension"               "ArithmeticalExpression"
"BaseDomain"                       "Complex"
"DerivativeProduct"                "DifferentialExpression"
"DifferentialFunction"             "DifferentialPolynomial"
"DifferentialProduct"              "DifferentialVariable"
"DihedralGroup"                    "DiscreteSet"
"DistributedPolynomial"            "Expression"
"ExpressionField"                  "ExteriorAlgebra"
"ExteriorProduct"                  "Float"
"Fraction"                         "FreeModule"
"GaloisField"                      "Ideal"
"ImageSet"                         "Integer"
"IntegerMod"                       "Interval"
"IntervalIntersection"             "IntervalSet"
"JetVectorField"                   "LinearDifferentialFunction"
"LinearDifferentialOperator"       "LinearOrdinaryDifferentialOperator"
"Matrix"                           "MatrixGroup"
"MonoidAlgebra"                    "MonoidOperatorAlgebra"
"MonomOrdering"                    "Multiset"
"MultivariatePolynomial"           "Numerical"
"OldDistributedPolynomial"         "OldPolynomial"
"Pade"                             "PermutationGroup"
"Polynomial"                       "PowerProduct"
"Product"                          "Quaternion"
"Rational"                         "Real"
"RestrictedDifferentialExpression" "SparseMatrix"
"SparseMatrixF2"                   "SquareMatrix"
"SubSet"                           "TensorAlgebra"
"TensorProduct"                    "TruncatedPowerSeries"
"UnivariatePolynomial"             "UnivariateRationalFunctions"
"UnivariateSkewPolynomial"         "VectorField"
))

("Network" .
(
"addEdge"      "addVertex"  "admissibleFlow" "allShortPath"    "blockflow" "changeEdge"
"changeVertex" "complete"   "convertSSQ"     "cycle"           "delEdge"   "delVertex"
"eCapacity"    "eWeight"    "edge"           "epost"           "epre"      "inDegree"
"isEdge"       "isVertex"   "longPath"       "maxFlow"         "minCost"   "minCut"
"outDegree"    "printGraph" "random"         "residualNetwork" "shortPath" "shortPathTo"
"showGraph"    "topSort"    "vWeight"        "vertex"
))

("Pref" .
(
"alias"       "callBack"     "callOnExit" "complete"        "debugOnTheFly" "echo"
"floatFormat" "keepOrder"    "kernel"     "matrixSeparator" "maxMem"        "maxTime"
"moduleTrace" "noProcRemTab" "output"     "postInput"       "postOutput"    "printTimesDot"
"prompt"      "promptString" "report"     "trailingZeroes"  "typeCheck"     "userOptions"
"verboseRead" "warn15"
))

("RGB" .
(
"AliceBlue"          "AlizarinCrimson"    "Antique"          "Aquamarine"
"AquamarineMedium"   "AureolineYellow"    "Azure"            "Banana"
"Beige"              "Bisque"             "Black"            "BlanchedAlmond"
"Blue"               "BlueLight"          "BlueMedium"       "BlueViolet"
"Brick"              "Brown"              "BrownOadder"      "BrownOchre"
"Burlywood"          "BurntSienna"        "BurntUmber"       "Cadet"
"CadmiumLemon"       "CadmiumOrange"      "CadmiumRedDeep"   "CadmiumRedLight"
"CadmiumYellow"      "CadmiumYellowLight" "Carrot"           "Cerulean"
"Chartreuse"         "Chocolate"          "ChromeOxideGreen" "CinnabarGreen"
"Cobalt"             "CobaltGreen"        "CobaltVioletDeep" "ColdGray"
"ColorNames"         "Coral"              "CoralLight"       "CornflowerBlue"
"Cornsilk"           "Cyan"               "CyanWhite"        "DarkOrange"
"DeepOchre"          "DeepPink"           "DimGray"          "DodgerBlue"
"Eggshell"           "EmeraldGreen"       "EnglishRed"       "Firebrick"
"Flesh"              "FleshOchre"         "Floral"           "ForestGreen"
"Gainsboro"          "GeraniumLake"       "Ghost"            "Gold"
"GoldOchre"          "Goldenrod"          "GoldenrodDark"    "GoldenrodLight"
"GoldenrodPale"      "Gray"               "Green"            "GreenDark"
"GreenPale"          "GreenYellow"        "GreenishUmber"    "Honeydew"
"HotPink"            "IndianRed"          "Indigo"           "Ivory"
"IvoryBlack"         "Khaki"              "KhakiDark"        "LampBlack"
"Lavender"           "LavenderBlush"      "LawnGreen"        "LemonChiffon"
"LightBeige"         "LightGoldenrod"     "LightGray"        "LightSalmon"
"LimeGreen"          "Linen"              "MadderLakeDeep"   "Magenta"
"ManganeseBlue"      "Maroon"             "MarsOrange"       "MarsYellow"
"Melon"              "MidnightBlue"       "Mint"             "MintCream"
"MistyRose"          "Moccasin"           "NaplesYellowDeep" "Navajo"
"Navy"               "NavyBlue"           "OldLace"          "Olive"
"OliveDrab"          "OliveGreenDark"     "Orange"           "OrangeRed"
"Orchid"             "OrchidDark"         "OrchidMedium"     "PapayaWhip"
"Peach"              "PeachPuff"          "Peacock"          "PermanentGreen"
"PermanentRedViolet" "Peru"               "Pink"             "PinkLight"
"Plum"               "PowderBlue"         "PrussianBlue"     "Purple"
"PurpleMedium"       "Raspberry"          "RawSienna"        "RawUmber"
"Red"                "RoseMadder"         "RosyBrown"        "RoyalBlue"
"SaddleBrown"        "Salmon"             "SandyBrown"       "SapGreen"
"SeaGreen"           "SeaGreenDark"       "SeaGreenLight"    "SeaGreenMedium"
"Seashell"           "Sepia"              "Sienna"           "SkyBlue"
"SkyBlueDeep"        "SkyBlueLight"       "SlateBlue"        "SlateBlueDark"
"SlateBlueLight"     "SlateBlueMedium"    "SlateGray"        "SlateGrayDark"
"SlateGrayLight"     "Smoke"              "Snow"             "SpringGreen"
"SpringGreenMedium"  "SteelBlue"          "SteelBlueLight"   "TerreVerte"
"Thistle"            "Titanium"           "Tomato"           "Turquoise"
"TurquoiseBlue"      "TurquoiseDark"      "TurquoiseMedium"  "TurquoisePale"
"Ultramarine"        "UltramarineViolet"  "VanDykeBrown"     "VenetianRed"
"Violet"             "VioletDark"         "VioletRed"        "VioletRedMedium"
"VioletRedPale"      "ViridianLight"      "WarmGray"         "Wheat"
"White"              "Yellow"             "YellowBrown"      "YellowGreen"
"YellowLight"        "YellowOchre"        "Zinc"
))

("Series" .
(
"Puiseux" "asympt" "gseries"
))

("Type" .
(
"AnyType"    "Arithmetical" "Complex"      "Divs"         "Equation"   "Even"
"Fraction"   "Imaginary"    "IndepOf"      "IntImaginary" "Integer"    "Interval"
"Irrational" "ListOf"       "ListOfIdents" "ListProduct"  "NegInt"     "NegRat"
"Negative"   "NonNegInt"    "NonNegRat"    "NonNegative"  "NonZero"    "Numeric"
"Odd"        "PolyOf"       "PosInt"       "PosRat"       "Positive"   "Prime"
"Product"    "Rational"     "RealNum"      "Relation"     "SequenceOf" "Series"
"SetOf"      "Singleton"    "TableOfEntry" "TableOfIndex" "Union"      "Unknown"
"Zero"
))

("adt" .
(
"Heap" "Queue" "Stack" "Tree"
))

("combinat" .
(
"bell" "cartesian" "composition" "partitions" "permute" "powerset" "stirling1" "stirling2"
))

("detools" .
(
"detSys"  "divergence" "euler" "genDetSys" "hasHamiltonian" "hasPotential" "modode" "ncDetSys"
"odeplot" "phaseplot"
))

("domains" .
(
"hasProp" "show_type"
))

("fp" .
(
"apply" "bottom" "curry" "fixargs" "fixedpt" "fold" "nest" "nestvals" "unapply"
))

("generate" .
(
"C" "TeX" "fortran"
))

("groebner" .
(
"dimension" "gbasis" "normalf" "spoly"
))

("import" .
(
"readdata" "readlisp"
))

("intlib" .
(
"RischIntegration" "changevar" "int" "lookup" "simplifyComplexIntegral"
))

("linalg" .
(
"Cholesky"       "GaussElim"      "GaussJordan"     "HermiteForm"   "Hessian"
"Hilbert"        "Jacobian"       "JordanForm"      "Sylvester"     "VandermondeSolve"
"addCol"         "addRow"         "adjoint"         "angle"         "basis"
"charMatrix"     "charPolynomial" "col"             "concatMatrix"  "crossProduct"
"curl"           "delCol"         "delRow"          "det"           "dimen"
"divergence"     "eigenValues"    "eigenVectors"    "expr2Matrix"   "extractMatrix"
"factorLU"       "factorQR"       "freeSet"         "grad"          "intBasis"
"inverseHilbert" "inverseLU"      "isHermitian"     "isOrthogonal"  "isPosDef"
"linearSolve"    "linearSolveLU"  "multCol"         "multRow"       "ncols"
"nonZeros"       "normalize"      "nrows"           "nullSpace"     "ogCoordTab"
"ogSystem"       "onSystem"       "permanent"       "pseudoInverse" "randomMatrix"
"rank"           "row"            "scalarProduct"   "setCol"        "setRow"
"stackMatrix"    "sumBasis"       "swapCol"         "swapRow"       "tr"
"transpose"      "vectorDimen"    "vectorPotential"
))

("linopt" .
(
"corners" "maximize" "minimize" "plot_data"
))

("listlib" .
(
"_concat" "append"        "insert"      "insertAt" "merge" "removeDupSorted" "removeDuplicates"
"revert"  "setDifference" "singleMerge" "sublist"
))

("misc" .
(
"breakmap" "genassop" "maprec" "minimalDomain" "subsFreeVars"
))

("module" .
(
"age" "displace" "func" "help" "load" "max" "stat" "which"
))

("numeric" .
(
"Bairstow"       "Butcher"         "GLdata"            "GTdata"      "Lagrange"
"Lagrange2d"     "NCdata"          "Newton"            "bisect"      "cubicSpline"
"det"            "eigenvalues"     "eigenvectors"      "expMatrix"   "fMatrix"
"factorCholesky" "factorLU"        "factorQR"          "fft"         "fint"
"fsolve"         "ifft"            "inverse"           "linearSolve" "linsolve"
"minpoly"        "odesolve"        "odesolveGeometric" "quadrature"  "rational"
"singularvalues" "singularvectors" "vonMises"
))

("numlib" .
(
"Lambda"           "Omega"      "contfrac" "decimal"    "divisors"    "ecm"
"fibonacci"        "fromAscii"  "g_adic"   "ichrem"     "igcdmult"    "ispower"
"isquadres"        "issqr"      "jacobi"   "lambda"     "legendre"    "lincongruence"
"mersenne"         "moebius"    "mpqs"     "mroots"     "msqrts"      "numdivisors"
"numprimedivisors" "omega"      "order"    "pollard"    "prevprime"   "primedivisors"
"primroot"         "proveprime" "sigma"    "sqrt2cfrac" "sumdivisors" "tau"
"toAscii"
))

("ode" .
(
"Wronskian"           "dAlembert"         "evalOde"              "exponentialSolutions"
"isFuchsian"          "isLODE"            "liouvillianSolutions" "normalize"
"polynomialSolutions" "rationalSolutions" "simplifySolutions"    "symmetricPower"
"unimodular"          "vectorize"
))

("orthpoly" .
(
"chebyshev1" "chebyshev2" "curtz" "gegenbauer" "hermite" "jacobi" "laguerre" "legendre"
))

("output" .
(
"ordinal" "tableForm" "tree"
))

("plotlib" .
(
"Lsys"            "Turtle"        "animate"     "animate2d" "animate3d"    "contourplot"
"cylindricalplot" "dataplot"      "densityplot" "fieldplot" "implicitplot" "inequalityplot"
"plot2d"          "plot3d"        "plotfunc"    "plotsetup" "polarplot"    "polygonplot"
"show_animation"  "sphericalplot" "xrotate"     "yrotate"
))

("polylib" .
(
"Dpoly"     "Poly"       "coeff"            "content"  "cyclotomic" "decompose"
"discrim"   "divide"     "elemSym"          "expr"     "factor"     "lcoeff"
"nthcoeff"  "poly"       "primitiveElement" "primpart" "randpoly"   "representByElemSym"
"resultant" "splitfield" "sqrfree"
))

("prog" .
(
"allCompletions" "calltree"        "check" "checkexample" "exprtree" "makeBinLib" "optimize"
"profile"        "setcheckglobals" "tcov"  "test"         "testfunc" "testinit"   "trace"
"untrace"
))

("property" .
(
"Fail" "Null" "delrule" "getrule" "reset" "setrule"
))

("solvelib" .
(
"BasicSet" "algebraic" "iroots" "isolate" "pdioe" "solve_poly"
))

("specfunc" .
(
"Ci"      "Si"        "abs"      "arccos"   "arccosh"  "arccot"  "arccoth"   "arccsc"  "arccsch"
"arcsec"  "arcsech"   "arcsin"   "arcsinh"  "arctan"   "arctanh" "bernoulli" "besselJ" "besselK"
"besselY" "beta"      "binomial" "catalan"  "cos"      "cosh"    "cot"       "coth"    "csc"
"csch"    "dilog"     "dirac"    "eint"     "erf"      "erfc"    "exp"       "fact"    "floor"
"gamma"   "heaviside" "igamma"   "lambertv" "lambertw" "ln"      "phi"       "polylog" "psi"
"sec"     "sign"      "sin"      "sinh"     "sqrt"     "tan"     "tanh"      "zeta"
))

("stats" .
(
"BPCorr"    "ChiSquare" "FCorr"    "a_quantil" "calc"   "col"         "concatCol" "concatRow"
"geometric" "harmonic"  "kurtosis" "linReg"    "mean"   "median"      "modal"     "normal"
"obliquity" "quadratic" "reg"      "row"       "sample" "sample2list" "selectRow" "sortSample"
"stdev"     "tabulate"  "unzipCol" "variance"  "zipCol"
))

("stdlib" .
(
"D"               "Factor"    "Im"          "Indets"    "O"           "Re"          "RootOf"
"assign"          "assume"    "bytes"       "close"     "coeff"       "collect"     "combine"
"complexInfinity" "conjugate" "contains"    "content"   "denom"       "diff"        "discont"
"expand"          "export"    "expose"      "expr"      "expr2text"   "factor"      "factors"
"fprint"          "freeze"    "gcd"         "gcdex"     "genident"    "getprop"     "hastype"
"icontent"        "igcd"      "igcdex"      "ilcm"      "infinity"    "info"        "int"
"iquo"            "irem"      "irreducible" "is"        "isprime"     "ithprime"    "lcm"
"lcoeff"          "length"    "limit"       "linsolve"  "loadlib"     "map"         "maprat"
"max"             "min"       "normal"      "nthcoeff"  "numer"       "open"        "partfrac"
"piecewise"       "plot2d"    "plot3d"      "plotfunc"  "poly2list"   "powermod"    "print"
"product"         "protocol"  "radsimp"     "random"    "rationalize" "read"        "rectform"
"revert"          "rewrite"   "rtime"       "select"    "series"      "setuserinfo" "simplify"
"solve"           "split"     "strmatch"    "substring" "sum"         "system"      "taylor"
"text2expr"       "time"      "unassume"    "undefined" "unexport"    "unfreeze"    "universe"
"unloadlib"       "version"   "zip"
))

("stringlib" .
(
"_concat" "contains"  "delete" "format" "formatf" "length" "pos" "revert" "strmatch" "subs"
"subsop"  "substring"
))

("student" .
(
"GaussElim" "Kn" "equateMatrix" "genExample" "isFree"
))

("transform" .
(
"fourier" "ifourier" "ilaplace" "laplace" "mellin"
))

))

(message "loadling mupad-cpl done")

(provide 'mupad-cpl)