% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrangle_items.R
\name{wrangle_items}
\alias{wrangle_items}
\alias{wrangle_items.sk_items}
\alias{wrangle_items.l500_items}
\alias{wrangle_items.l300_items}
\title{Wrangle items imported from different instrument databases into a common data structure}
\usage{
wrangle_items(x, ...)

\method{wrangle_items}{sk_items}(x, ...)

\method{wrangle_items}{l500_items}(x, ...)

\method{wrangle_items}{l300_items}(x, ...)
}
\arguments{
\item{x}{data imported from an instrument database}

\item{...}{additional arguments used by methods}
}
\value{
a tibble with columns ... (WIP).
}
\description{
Wrangle items imported from different instrument databases into a common data structure
}
\section{Methods (by class)}{
\itemize{
\item \code{wrangle_items(sk_items)}: method

\item \code{wrangle_items(l500_items)}: method

\item \code{wrangle_items(l300_items)}: method

}}
