% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subsample.R
\name{subsample}
\alias{subsample}
\title{Subsample}
\usage{
subsample(
  file,
  algorithm = "SPATIAL",
  parameter = 0.01,
  output_dir = "C:\\\\output.las",
  global_shift = F,
  global_shift_type = "AUTO",
  c_export_fmt = "LAS",
  c_ext = "las",
  silent = T,
  no_timestamp = T
)
}
\arguments{
\item{file}{a}

\item{algorithm}{a}

\item{parameter}{a}

\item{output_dir}{a}

\item{global_shift}{a}

\item{global_shift_type}{a}

\item{c_export_fmt}{a}

\item{c_ext}{a}

\item{silent}{a}

\item{no_timestamp}{a}
}
\value{

}
\description{
Subsample
}
\examples{
\dontrun{
#' pc_files <- list.files('C://', pattern='.las$',full.names=T)
CC(merge_clouds(files= pc_files))
}

}
