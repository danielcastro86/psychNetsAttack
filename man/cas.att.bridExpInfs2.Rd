\name{cas.att.bridExpInfs2}
\alias{cas.att.bridExpInfs2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Cascade Attack - Bridge Expected Influence Two-Step}
\description{
Estimates Bridge Expected Influence two-step centrality for the initial network and after each node removal.  The node with highest centrality is removed and network properties are estimated.}
\usage{
cas.att.bridExpInfs2(g, adjmat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{igraph network}
  \item{adjmat}{adjacency matrix of the network}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A dataframe with the network properties values for the initial network and after each node removal.
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
