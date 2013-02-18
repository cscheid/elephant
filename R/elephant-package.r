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
    saveRDS(value, name, FALSE)
    add_file_to_head(elephant$repository, name, key)
    unlink(name)
  }
}

ask.elephant <- function(elephant, key)
{
  value.path <- str_c(elephant$repository$workdir(),"/",key)
  if (!file.exists(value.path))
    stop("key not found")
  result <- readRDS(value.path)
  result
}
