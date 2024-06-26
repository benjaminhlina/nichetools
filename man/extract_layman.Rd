% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_layman.R
\name{extract_layman}
\alias{extract_layman}
\title{extract Layman metrics}
\usage{
extract_layman(data, community_df = NULL, data_format = NULL)
}
\arguments{
\item{data}{a \code{list} created by the function \code{bayesianLayman()} from the package
\href{https://CRAN.R-project.org/package=SIBER}{{SIBER}}.}

\item{community_df}{a two column data frame. One of the columns has to be named
\code{community} and the data in the column will be \code{numerics} as a \code{character}
string(e.g., \verb{"1", "2", "3"}). This is the order of the community names
and will be used to join the actual community names to the correct data.
These are the same class and values required by the function, \code{createSiberObject()}
from \href{https://CRAN.R-project.org/package=SIBER}{{SIBER}}.
The second column contains the actual names of the communities
that the user is working with (e.g., \code{"region"}).}

\item{data_format}{a \code{character} string that decides whether the returned object is
in long or wide format. Default is \code{"long"}, with the alternative supplied being \code{"wide"}.}
}
\value{
A \code{tibble} containing four rows when \code{data_format} is set to its
default which is \code{long}. These four rows are the following, \code{community},
\code{the_name_of_the_communities}, \code{metric} and \code{post_est}.
}
\description{
Extract Bayesian estimates for the following six layman metrics,
\eqn{\delta^{13}}C range,  \eqn{\delta^{15}}N range, total area (TA),
distance to centroid (CD), distance to the nearest neighbour (NND), and
the standard deviation of the distance to the nearest neighbour (SDNND)
from data objects created by
\href{https://CRAN.R-project.org/package=SIBER}{{SIBER}}. To learn
more about the following metrics please review
\href{https://esajournals.onlinelibrary.wiley.com/doi/full/10.1890/0012-9658\%282007\%2988\%5B42\%3ACSIRPF\%5D2.0.CO\%3B2}{Layman et al. (2008)}.
}
\examples{

library(nichetools)
library(SIBER)

# ---- bring in SIBER demo data ----
# uncomenet to use
# str(demo.siber.data)

# ---- create coummnity names data frame ----
# uncomment to use
# str(demo.siber.data.2)

demo.siber.data.2$community_names <- as.factor(demo.siber.data.2$community)

demo.siber.data.2$community <- as.numeric(demo.siber.data.2$community_names) |>
as.character()
c_names <- demo.siber.data.2 |>
dplyr::distinct(community, community_names)

# ---- create the siber object ----
siber.example <- createSiberObject(demo.siber.data)

# ---- view Baysain estimates of mu and sigma produced by SIBER ---
# uncomment to use
# str(post_sam_siber)

# ---- extract posterior estimates of mu -----

mu_post <- extractPosteriorMeans(siber.example, post_sam_siber)

# ---- Bayesian estimates of layman metrics using SIBER ----

layman_b <- bayesianLayman(mu.post = mu_post)

# ---- use nichetools to extract Baysian estimats of Layman metrics ----

layman_be <- extract_layman(layman_b, community_df = c_names)

layman_be

}
\seealso{
\code{\link[SIBER:bayesianLayman]{SIBER::bayesianLayman()}} and \code{\link[SIBER:createSiberObject]{SIBER::createSiberObject()}}
}
