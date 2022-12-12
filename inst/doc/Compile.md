

Using a sample of 1000 of the files in time.R, we profile the calls to 
`to_ast(parse(f))` for all of them to find what functions take most time:

```
                         self.time self.pct total.time total.pct
"ArgumentList$new"            6.42    12.53      38.32     74.79
"initialize"                  6.04    11.79      49.06     95.75
"...length"                   4.84     9.45       4.84      9.45
"Symbol$new"                  4.36     8.51       8.08     15.77
"to_ast.call"                 4.00     7.81      44.20     86.26
"super$initialize"            2.88     5.62      49.06     95.75
"lapply"                      2.86     5.58      51.24    100.00
"$<-"                         1.82     3.55       1.82      3.55
"to_ast.numeric"              1.68     3.28       2.10      4.10
"<Anonymous>"                 1.54     3.01       6.10     11.90
"print.default"               1.52     2.97       1.52      2.97
"list"                        1.48     2.89       1.48      2.89
"new.env"                     1.32     2.58       1.32      2.58
"FUN"                         1.24     2.42      51.24    100.00
"Call$new"                    1.14     2.22      12.56     24.51
"set_parent"                  1.08     2.11       3.26      6.36
"makeActiveBinding"           0.72     1.41       0.96      1.87
"set_parent.ASTNode"          0.58     1.13       0.78      1.52
"is.function"                 0.50     0.98       0.50      0.98
"to_ast"                      0.38     0.74      49.08     95.78
"parse"                       0.38     0.74       0.64      1.25
"Character$new"               0.36     0.70       0.58      1.13
"to_ast.name"                 0.34     0.66       6.06     11.83
"Numeric$new"                 0.30     0.59       0.42      0.82
"as.list.default"             0.26     0.51       0.26      0.51
"as.name"                     0.24     0.47       0.24      0.47
"Assignment$new"              0.18     0.35       1.28      2.50
"set_parent.list"             0.16     0.31       1.90      3.71
"Subset1$new"                 0.14     0.27       1.70      3.32
"SubsetDollar$new"            0.14     0.27       1.26      2.46
"match.fun"                   0.14     0.27       0.64      1.25
"as.list"                     0.14     0.27       0.40      0.78
"Literal.initialize"          0.14     0.27       0.30      0.59
"$"                           0.14     0.27       0.14      0.27
"file"                        0.14     0.27       0.14      0.27
"Brace$new"                   0.12     0.23      49.08     95.78
"list_dots_safely"            0.12     0.23      31.40     61.28
"Call.initialize"             0.12     0.23       2.48      4.84
"If$new"                      0.10     0.20       1.90      3.71
"readLines"                   0.10     0.20       0.10      0.20
"inherits"                    0.08     0.16      48.92     95.47
"as.character"                0.08     0.16       0.08      0.16
"to_ast.<-"                   0.06     0.12      34.04     66.43
"getClassDef"                 0.06     0.12       0.20      0.39
"is.character"                0.06     0.12       0.06      0.12
"is.list"                     0.06     0.12       0.06      0.12
"Assignment.initialize"       0.04     0.08       0.82      1.60
"Function$new"                0.04     0.08       0.70      1.37
"EmptyArgument$new"           0.04     0.08       0.62      1.21
"mapply"                      0.04     0.08       0.10      0.20
"c"                           0.04     0.08       0.04      0.08
"methodsPackageMetaName"      0.04     0.08       0.04      0.08
"nzchar"                      0.04     0.08       0.04      0.08
"Parenthesis$new"             0.02     0.04       3.92      7.65
"to_ast.character"            0.02     0.04       0.60      1.17
"to_ast_parameters"           0.02     0.04       0.36      0.70
"Return$new"                  0.02     0.04       0.16      0.31
"wrap_brace"                  0.02     0.04       0.16      0.31
"Label$new"                   0.02     0.04       0.08      0.16
"getPackageName"              0.02     0.04       0.06      0.12
"Parameter$new"               0.02     0.04       0.06      0.12
"rlang::dots_list"            0.02     0.04       0.04      0.08
"!"                           0.02     0.04       0.02      0.04
".Call"                       0.02     0.04       0.02      0.04
".identC"                     0.02     0.04       0.02      0.04
".requirePackage"             0.02     0.04       0.02      0.04
"close.connection"            0.02     0.04       0.02      0.04
"environmentName"             0.02     0.04       0.02      0.04
"packageSlot"                 0.02     0.04       0.02      0.04
"parent.frame"                0.02     0.04       0.02      0.04
"paste0"                      0.02     0.04       0.02      0.04
"stopifnot"                   0.02     0.04       0.02      0.04
"to_ast.ASTNode"              0.02     0.04       0.02      0.04
```


The number of calls to each new() function (for a different sample of 1000 files) are
```
           Symbol      ArgumentList              Call 
           372319            347584            138196 
          Numeric         Character        Assignment 
            46881             46351             33989 
     SubsetDollar           Subset1             Brace 
            18143             15388             13671 
    EmptyArgument         Parameter       Parenthesis 
            10224              5611              5509 
          Logical                If ReplacementDollar 
             5330              4183              3379 
     Replacement1          Function     ParameterList 
             3116              2653              2653 
            Label           Subset2         Namespace 
             2597              1778              1493 
              For       Replacement            Return 
             1378              1123              1013 
             Null      Replacement2   SuperAssignment 
              876               303               144 
            While              Next           Integer 
               99                77                62 
            Break               Phi         SCCHelper 
               30                 0                 0 
        Primitive        Invocation            Branch 
                0                 0                 0 
           Subset          Callable           Complex 
                0                 0                 0 
         Internal       ControlFlow         Container 
                0                 0                 0 
        BlockList             Stack           ASTNode 
                0                 0                 0 
          Counter              Loop           Literal 
                0                 0                 0 
ConditionalBranch             Block 
                0                 0 
```
