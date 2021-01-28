\name{nSampleNet}
\alias{nSampleNet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{mean global network descriptives}
\description{
mean global network descriptives with random sample selection
}
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
%%  ~~who you are~~
}
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
