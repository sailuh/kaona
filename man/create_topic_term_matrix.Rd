% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/topic.R
\name{create_topic_term_matrix}
\alias{create_topic_term_matrix}
\title{Create Topic-Term Matrix}
\usage{
create_topic_term_matrix(n_runs, corpus, n_clusters, alpha, beta, stop_words)
}
\arguments{
\item{n_runs}{The number of runs to perform topic modeling}

\item{corpus}{The corpus used for WarpLDA}

\item{n_clusters}{The number of topics/clusters chosen}

\item{alpha}{Alpha parameter as specified in WarpLDA}

\item{beta}{Beta parameter as specified in WarpLDA}

\item{stop_words}{The list of stop words to be used}
}
\description{
Create Topic-Term Matrix
}
