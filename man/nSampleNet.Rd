\name{nSampleNet}
\alias{nSampleNet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{random sample selection from data and net descriptives estimation}
\description{
This function allows for the extraction of a user-specified sample size
from the data, a n number of times. The function estimates the network either
through BGGM for ordinal variables or ggmModSelect. The function returns net
descriptives from all the random n samples retrieved and the mean global descriptives
for the global network properties from netDesc function}
\usage{
nSampleNet(nsamples, samplesize, data, method)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nsamples}{number of samples to extract}
  \item{samplesize}{Size of of the sample}
  \item{data}{data from where the samples should be exracted}
  \item{method}{"ggmModSelect" estimation with bootnet or "BGGM1" estimation
  with BGGM for ordinal variables}
%%     ~~Describe \code{x} here~~
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
mean values for global network properties and netDesc results
for all samples

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Daniel Castro}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
