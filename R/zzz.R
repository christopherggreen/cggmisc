# various utility functions

# previously we imported the splus2R package
# for these, but that creates lots of conflicts.
# they are simple enough to define internally

anyMissing <- function(x) any(is.na(unlist(x)))
which.na   <- function(x) which(is.na(x))

# for historical compatibility with other 
# scripts
memsize <- memory.size
