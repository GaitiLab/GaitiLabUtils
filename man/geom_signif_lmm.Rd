% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_signif_lmm.R
\name{geom_signif_lmm}
\alias{geom_signif_lmm}
\title{geom signif with lmm test. See geom_signif from ggsignif for more information}
\usage{
geom_signif_lmm(
  data_df,
  response,
  condition,
  latent_vars,
  method = "nested_t_test",
  comparisons = NULL,
  y_position = NULL,
  tip_length = 0.03,
  size = 0.5,
  step_increase = 0,
  verbose = TRUE
)
}
\arguments{
\item{data_df}{data frame containing data}

\item{response}{name of column in data_df that corresponds to the response variable}

\item{condition}{name of column in data_df that corresponds to condition of interest}

\item{latent_vars}{vector containing columns in data_df that correspond to variables that describe the nested structure of the data e.g. Patient}

\item{method}{one of the following 'nested_t_test' or 'mixed_effects_anova' (default="nested_t_test")}

\item{comparisons}{list of vectors of length two that define the comparisons to make (default=NULL)}

\item{y_position}{position of brackets (default=NULL)}

\item{tip_length}{length of bracket tips (default=0.03)}

\item{size}{size of brackets (default=0.5)}

\item{step_increase}{distance between brackets (default=0)}

\item{verbose}{(default=TRUE)}
}
\value{
geom signif
}
\description{
geom signif with lmm test. See geom_signif from ggsignif for more information
}
