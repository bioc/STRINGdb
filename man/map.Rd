\name{map}
\alias{\S4method{map}{STRINGdb}}
\alias{map}
\title{
map
}
\description{
Maps the gene identifiers of the input dataframe to STRING identifiers.
It returns the input dataframe with the "STRING_id" additional column.
}
\usage{
\S4method{map}{STRINGdb}(my_data_frame, my_data_frame_id_col_names, takeFirst=TRUE, removeUnmappedRows=FALSE, quiet=FALSE, usePreferredSources=TRUE)
}
\arguments{
  \item{my_data_frame}{
data frame provided as input.
}
  \item{my_data_frame_id_col_names}{
vector contatining the names of the columns of "my_data_frame" that have to be used for the mapping.
}
  \item{takeFirst}{
 boolean indicating what to do in case of multiple STRING proteins that map to the same name. 
 If TRUE, only the first of those is taken. Otherwise all of them are used. (default TRUE)
}
  \item{removeUnmappedRows}{
remove the rows that cannot be mapped to STRING 
(by default those lines are left and their STRING_id is set to NA).
}
  \item{quiet}{
Setting this variable to TRUE we can avoid printing the warning relative to the unmapped values.
}
  \item{usePreferredSources}{
when \code{takeFirst=TRUE}, prioritize aliases using the preferred source order.
}

}
\value{
Returns the dataframe that is given in input with the "STRING_id" additional column.
The first column of \code{my_data_frame_id_col_names} comes first, followed by the other columns of the input and by "STRING_id".
The identifiers are compared without regard to case. An NA or empty identifier is never mapped (its STRING_id is NA).
The rows keep the order of the input, also when several identifier columns are used, and each row keeps its own identifiers as given;
with \code{takeFirst=FALSE}, a row whose identifier matches several STRING proteins is repeated, once for each of them.
}
\author{
Andrea Franceschini
}

