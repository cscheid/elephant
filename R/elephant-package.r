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
    remove_file_from_head(.subset2(elephant, "repository"), key)
  } else {
    name <- tempfile()
    saveRDS(value, name, FALSE)
    add_file_to_head(.subset2(elephant, "repository"), name, key)
    unlink(name)
  }
}

ask.elephant <- function(elephant, key)
{
  value.path <- str_c(.subset2(elephant, "repository")$workdir(),"/",key)
  if (!file.exists(value.path))
    stop("key not found")
  result <- readRDS(value.path)
  result
}

"$.elephant" <- function(elephant, key)
{
  ask.elephant(elephant, key)
}

"$<-.elephant" <- function(elephant, key, value)
{
  tell.elephant(elephant, key, value)
  elephant
}

names.elephant <- function(elephant)
{
  index <- .subset2(elephant, "repository")$index()
  count <- index$entrycount()
  vapply(0:(count-1), function(i) { index$get_by_index(i)$path }, c(""))
}
