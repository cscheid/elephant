#' elephant
#'
#' @name elephant
#' @docType package

open.elephant <- function(path.to.backend)
{
}

new.elephant <- function(path.to.backend)
{
  result <- list()
  result$repository <- repository_init(path.to.backend, FALSE)
  class(result) <- c("elephant")
  result
}

tell.elephant <- function(elephant, key, value)
{
  if (is.null(value)) {
    remove_file_from_head(elephant$repository, key)
  } else {
    name <- tempfile()
    tf <- file(name, "wb")
    serialize(value, tf, FALSE)
    close(tf)
    add_file_to_head(elephant$repository, name, key)
    unlink(name)
  }
}

ask.elephant <- function(elephant, key)
{
  value <- file(str_c(elephant$repository$workdir(),"/",key), "rb")
  if (is.null(value))
    stop("key not found")
  result <- unserialize(value)
  close(value)
  result
}

