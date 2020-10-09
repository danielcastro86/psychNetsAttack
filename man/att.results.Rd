\name{att.results}
\alias{att.results}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Attack Results}
\description{This function estimates the peak outcome and the attack extent
for the number of components and the average path length of the network. In addition
it shows the number of components, the average path length and the density for a
user specified number of nodes in the network.}
\usage{
att.results(attackscores, attacktype, centmeas, totalvertex, remainingvertex,
            randomscores)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{attackscores}{data from the performed attack}
  \item{attacktype}{Type of attack used. cascade or normal}
  \item{centmeas}{Centrality measure to analyze, strength, eigenvector, bridge strength, degree, modal control, average control}
  \item{totalvertex}{Number of nodes in the network}
  \item{remainingvertex}{Number of nodes in the network to which the additional
  results should be calculated}
  \item{randomscores}{data from the random attack}
}
\details{All of the arguments must be specified}
\value{A list of 3 dataframes, attack results, network descriptives and random attack results}
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
