\name{updateR6}
\alias{updateR6}
\title{Update all R6 Class Definitions in a Package}
\description{
  This finds all the R6 class (generator) objects in a
  namespace, environment or list and applies the specified function
  to each. This can update the class definition, replacing
  implementations of methods, tracing methods, etc.
}
\usage{
updateR6(namespace, fun = mkNew, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{namespace}{the name of the package or namespace, or the environment itself.}  
  \item{fun}{the function to apply to each R6 class in the namespace.}
  \item{\dots}{additional arguments passed in each call to \code{fun}.}
}
\value{
  A list with an element for each R6 class in the namespace.
  Each element is the result of the call to \code{fun}
  with that R6 class as the first argument.
}
\references{
  The R6  package.
}
\author{
Duncan Temple Lang
}

\seealso{
  \code{\link{mkNew}}
}
%\examples{}
\keyword{programming}
\concept{efficiency}
