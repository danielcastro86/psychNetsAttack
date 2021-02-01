\name{attack}
\alias{attack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Network Attack}
\description{Execute the specified attack and the random attack}
\usage{
attack(attack, measure, graph)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{attack}{normal or cascade}
  \item{measure}{strength, bridge strength, average control, modal control, degree,
  eigenvector, bridge expected influence 1-step. bridge expected influence 2-step, expected influence 1-step, expected influence 2-step}
  \item{graph}{igraph network}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{dataframe scores for density, average path length, number of components, transitivity, centralization, and cluster sizes after each node removal
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
