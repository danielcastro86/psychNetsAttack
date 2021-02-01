\name{att.results}
\alias{att.results}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Attack Results}
\description{This function estimates the peak outcome and the attack extent for the number of components and the average path length of the network. In addition it shows the number of components, the average path length and the density of the network with half of the nodes.}
\usage{
att.results(attackscores)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{attackscores}{dataframes with scores from the performed attack}
}
\details{}
\value{
  \item{Magnitude}{Results for the performed attack and random attack magnitude}
  \item{Extension}{Results for the performed attack and random attack extension}
  \item{Half Nodes}{Results for the perfomed attack and random attack after removing half of the nodes}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Daniel Castro}
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
