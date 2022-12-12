\name{mkNew}
\alias{mkNew}
\title{Generate a `new()` function for an R6 class}
\description{
  This examines the R6 class's fields and methods and those of the
  super/base-classes and generates the class-specific code to construct
  an instance of this R6 class and invoke its initialize method.
}
\usage{
mkNew(k, update = TRUE)
}
\arguments{
  \item{k}{the \code{R6ClassGenerator} object describing the R6 class,
    created via a call to \code{\link[R6]{R6Class}}.}
  \item{update}{a logical value which, if \code{TRUE}, will insert
    the newly created \code{new} function directly into the R6 class
    for immediate use.
  }
}
\value{
  A function.
}
%\references{}
\author{
Duncan Temple Lang <duncan@r-project.org>
}
\seealso{
  \code{\link{updateR6}}
}
%\examples{
%\dontrun{
%}
%}
\keyword{programming}
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{meta-programming}
\concept{efficiency}
\concept{compilation}
