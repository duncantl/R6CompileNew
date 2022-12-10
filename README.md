## Making R6 `new()` Functions Faster.

The R6 package provides an alternative to R's own reference classes in the methods package.
It is used by a little over 100 packages on CRAN, and often in cases where there are other delays,
such as network requests.  Accordingly, making R6 code faster is not necessarily worth a significant
investment.

Nick Ulle (UC Davis) developed a static analysis package - rstatic - that provides a good foundation for
structured code analysis, and also another package for static type inference - RTypeInference. He built these
using R6 classes to represent the abstract syntax tree.

Unfortunately, the code is slow and some of this can be attributed to the R6 class mechanism.
Rather than rewrite the rstatic and RTypeInference packages (yet?), I decided to explore improving
the performance of rstatic and R6 with minimal changes to either.

One issue with R6 is that the new() function for a class is the same for every class.
It doesn't leverage knowledge of the class definition, but queries it.  This clearly
involves querying the same information in each call to the new() function for a given class.

Querying the class definition(s) can be done once.  The code in this package takes an R6 class
definition  (and those of its super-classes) and creates/"compiles" a specialized `new()` 
function for each class.
 
These class-specific `new()` functions can be created when

+ the class is defined, or
+ the package is being installed
+ the package is being loaded
+ anytime during an R session by the user.


## Resulting Speedup

Working on rstatic's 47 classes and parsing a somewhat randomly identified R file,
we currently get a speed up of a factor of 4 when using the `new()` functions.

This does not include the time to create the `new()` functions, which is 0.209 of a second.


## End-User Usage

```r
library(rstatic)
source("../rstaticDebug/compileNew2.R")
source("../rstaticDebug/updateNewFuns.R")
update(mkNew, TRUE, namespace = "rstatic")
```

