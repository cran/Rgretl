\name{description}
\alias{description}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
printing description of a data frame  
}
\description{
Prints "description" attribute of an object if such attribute exists. Otherwise returns \code{NULL}. It is intended to be applied to data frames created by \code{open_gdt}.
}
\usage{
description(x)
# denmark <- open_gdt("denmark")
# description(denmark)

}

\arguments{
  \item{x}{
any object, supposedly a data frame created by \code{open_gdt}. If \code{x} has \code{"description"} attribute the function prints its contents; otherwise returns \code{NULL}.}
}
\value{
character vector}



\author{
Oleh Komashko
}
\examples{

\dontrun{
denmark <- open_gdt("denmark")
description(denmark)
}
}