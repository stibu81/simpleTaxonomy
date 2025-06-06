% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{read_taxonomy}
\alias{read_taxonomy}
\title{Read Taxonomic Hierarchy From File}
\usage{
read_taxonomy(file, delim = ",")
}
\arguments{
\item{file}{path to the csv file}

\item{delim}{the delimiter used in the file}
}
\value{
a \code{taxonomy_graph} object which inherits from \code{igraph}
}
\description{
Read a csv file that contains the data that defines a taxonomic
hierarchy and return it as a graph.
}
\details{
The file must contain column names in the first row. Each row defines a
taxon with the following attributes:

\describe{
\item{parent}{The name of the parent taxon. This column must be empty for
the root taxon.}
\item{name}{The name of the taxon that should be used to label the nodes,
e.g., the common name. These names must be unique.}
\item{scientific}{The scientific name of the taxon.}
\item{rank}{The rank of the taxon, e.g., "Familie", "Art", or similar (only
German ranks are supported so far).
These will be used to colour the nodes.}
}

The function checks that the file satisfies the following conditions:
\itemize{
\item The required columns are all present.
\item There is exactly one root taxon, which is a taxon without parent.
\item There are no duplicated names.
\item Each parent taxon is defined by its own row in the table.
\item The rank is always defined.
}

In addition, the function also warns if an unknown rank is used. This will
still lead to a graph that can be plotted, but the nodes with unknown rank
will not be coloured.

The common names (column "name") of the taxa must be unique, because they
are used to create the graph. Sometimes, there are taxa that have identical
common names, e.g. the family Equidae and the genus Equus are both called
"Pferde" in German. In theses cases, one can use an additional identifier
in parenthesis to make the names unique, e.g., "Pferde (F)" and "Pferde" for
the family and the genus, respectively. The identifier "(F)" will be removed
and not be shown in the visualisation.
}
\examples{
file <- get_example_taxonomy_file()
taxonomy <- read_taxonomy(file)
class(taxonomy)
as_tibble(taxonomy)

}
