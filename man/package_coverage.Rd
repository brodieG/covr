% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/covr.R
\name{package_coverage}
\alias{package_coverage}
\title{Calculate test coverage for a package}
\usage{
package_coverage(path = ".", ..., type = c("test", "vignette", "example",
  "all", "none"), relative_path = TRUE, quiet = TRUE, clean = TRUE,
  exclusions = NULL, exclude_pattern = getOption("covr.exclude_pattern"),
  exclude_start = getOption("covr.exclude_start"),
  exclude_end = getOption("covr.exclude_end"), use_subprocess = TRUE)
}
\arguments{
\item{path}{file path to the package}

\item{...}{extra expressions to run}

\item{type}{run the package \sQuote{test}, \sQuote{vignette},
\sQuote{example}, \sQuote{all}, or \sQuote{none}. The default is
\sQuote{test}.}

\item{relative_path}{whether to output the paths as relative or absolute
paths.}

\item{quiet}{whether to load and compile the package quietly}

\item{clean}{whether to clean temporary output files after running.}

\item{exclusions}{a named list of files with the lines to exclude from each file.}

\item{exclude_pattern}{a search pattern to look for in the source to exclude a particular line.}

\item{exclude_start}{a search pattern to look for in the source to start an exclude block.}

\item{exclude_end}{a search pattern to look for in the source to stop an exclude block.}

\item{use_subprocess}{whether to run the code in a separate subprocess.
Needed for compiled code and many packages using S4 classes.}
}
\description{
Calculate test coverage for a package
}
\seealso{
exclusions
}

