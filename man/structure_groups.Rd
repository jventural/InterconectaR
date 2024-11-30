\name{structure_groups}
\alias{structure_groups}
\title{Create Structured Groups Based on Names and Values}

\description{
This function creates structured groups by assigning sequential indices to each group based on specified names and their respective sizes. The output is a structured list where each group name is associated with a sequence of indices.
}

\usage{
structure_groups(names, values)
}

\arguments{
  \item{names}{A character vector specifying the names of the groups.}
  \item{values}{A numeric vector indicating the size of each group. Each value determines the number of indices assigned to the corresponding group in \code{names}.}
}

\details{
The function ensures that the lengths of \code{names} and \code{values} are equal. For each group, it calculates the starting and ending indices based on the sizes specified in \code{values} and assigns these indices to the corresponding group in \code{names}.

The result is a structured list where each element is named according to \code{names} and contains a sequence of integers representing the group members. Additionally, the list is given a \code{Names} attribute, matching the input \code{names}.
}

\value{
A structured list where each element corresponds to a group name and contains a sequence of indices. The list also has an attribute \code{Names} equal to the input \code{names}.
}

\examples{
# Example usage with specific names and values
names <- c("Myths of love", "Satisfaction", "Negative Interaction",
           "Involvement", "Jealousy", "Infidelity")
values <- c(2, 1, 1, 1, 1, 4)

# Create structured groups
groups <- structure_groups(names, values)

# Print the result
print(groups)
# Output:
# $`Myths of love`
# [1] 1 2
#
# $Satisfaction
# [1] 3
#
# $`Negative Interaction`
# [1] 4
#
# $Involvement
# [1] 5
#
# $Jealousy
# [1] 6
#
# $Infidelity
# [1] 7 8 9 10
#
# Attributes:
# $Names
# [1] "Myths of love" "Satisfaction" "Negative Interaction"
#     "Involvement" "Jealousy" "Infidelity"
}

\author{
Dr. José Ventura-León
}
