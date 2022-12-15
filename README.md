## Making R6 `new()` Functions Faster.

The R6 package provides an alternative to R's own reference classes in the methods package.
It is used by a little over 100 packages on CRAN, and often in cases where there are other delays
such as network requests.  Furthermore, while reference classes are useful in some situations, the
side-effect-free nature of functional programming in R often leads to better code.  Mixing the two
models adds complexity to the computational model a user needs to consider. Accordingly, making R6
code faster is not necessarily worth a significant investment of time.

However, Nick Ulle (UC Davis) developed a static analysis package -
[rstatic](https://github.com/nick-ulle/rstatic) - that provides a good foundation for structured
code analysis, and also another package for static type inference -
[RTypeInference](https://github.com/duncantl/RTypeInference). And we use some of the functionality
in the [CodeAnalysis](https://github.com/duncantl/CodeAnalysis) package. Nick built the abstract
syntax tree underlying much of the first two packages using R6 classes to represent the abstract syntax tree.

Unfortunately, the code is slow and some of this can be attributed to the R6 class mechanism.
Rather than rewrite the rstatic and RTypeInference packages (yet?), I decided to explore improving
the performance of rstatic and R6 with minimal changes to either.

One issue with R6 is that the `new()` constructor function for a class is the same for every class.
It doesn't leverage knowledge of the class definition, but queries it.  This clearly
involves querying the same information in each call to the new() function for a given class.

Querying the class definition(s) can be done once.  The `mkNew()` function in this package takes an R6 class
definition  (and those of its super-classes) and creates/"compiles" a specialized `new()` 
function for that class. The `updateR6()` function can apply `mkNew()` (or other functions) on all
R6 classes in a package, environment or list.
 
These class-specific `new()` functions can be created when

+ the class is defined, or
+ the package is being installed,
+ the package is being loaded,
+ anytime during an R session by the user.


## Resulting Speedup

Compiling rstatic's 47 classes and then parsing a 5904 different R files from many github
repositories, the median speedup factor was exactly 5, with a standard deviation of .57.
However, it is a bimodal distribution with modes at 4.5 and 5.5.

This does not include the one time cost to create all the `new()` functions, which is about 0.140 of a second. 
<!-- 0.209 in one run.  .134 in a recent run. -->

<!--
Examining 98 relatively large files, we see a speedup factor ranging between 2.6 and 5.5.
With an original subset of 27 of these, we saw the range between 4.4 and 6.2, and a positive relationship
between the number of elements in the parsed R code.
So there is large variability in run-times, partially due to garbage collection.
For the 98 files, the median speedup is 4.43.
-->


### Caveats

We currently 
+ don't lock the bindings or environments
+ don't add code for any finalizers.

These are relatively easy to add to the generate code using the same approach in 
`mkNew()` and related helper functions.


## End-User Usage

```r
library(R6CompileNew)
library(rstatic)
updateR6("rstatic", update = TRUE)
```


## Further Opportunities

The clone and copy_slice functions become methods in R6, but  are also general for all 
classes.  We could use the same approach to create R6-class-specific  versions to make these go
faster.

We have not implemented this (yet) as we changed the use of copy() in rstatic (which calls the `clone()`
method) to a call to `to_ast()` which calls the `new()` constructor function instead.
