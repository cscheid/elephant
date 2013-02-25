#' elephant
#'
#' @name elephant
#' @docType package

open.elephant <- function(path.to.backend)
{
  result <- list()
  result$repository <- new(guitar::Repository, path.to.backend)
  if (result$repository$is_bare())
    stop("Only works on non-bare repositories")
  class(result) <- c("elephant")
  result
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

ask.elephant <- function(elephant, key, time=NULL)
{
  if (is.null(time)) {
    value.path <- str_c(.subset2(elephant, "repository")$workdir(),"/",key)
    if (!file.exists(value.path))
      stop("key not found")
    result <- readRDS(value.path)
  } else {
    commit <- locate.commit.by.time(elephant, time)
    tree <- commit$tree()
    vec <- tree$entry_bypath(key)$object()$data()
    result <- unserialize(vec)
  }
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

################################################################################

all.commits <- function(elephant)
{
  current_commit <- .subset2(elephant, "repository")$head()$peel(GIT_OBJ_COMMIT)
  ancestry <- list(current_commit)
  index <- 2
  while (current_commit$parent_count() > 0) {
    # assume no merges for now.
    current_commit <- current_commit$parent(0)
    ancestry[[index]] <- current_commit
    index <- index + 1
  }
  ancestry
}

to.posix.time <- function(git_time)
{
  .POSIXct(git_time$time)
}

# locate the latest commit before 'time'
locate.commit.by.time <- function(elephant, time)
{
  # when we cache the commit ancestry, the binary search here will
  # make sense right now it seems wasteful to create the whole vector
  # and then traverse it again every time

  # ASSUMPTION! time is monotonic on commits.  FIXME If people commit
  # data versions across leap seconds, this could be bad.
  
  commits <- all.commits(elephant)
  times <- lapply(commits, function(x) to.posix.time(x$time()))

  if (time > times[[1]]) {
    commits[[1]]
  } else if (time < times[[length(times)]]) {
    commits[[length(times)]]
  } else {
    left <- 1
    right <- length(times)
    center <- as.integer((left + right) / 2)
    
    while (right - left > 1) {
      if (times[[center]] > time) {
        left <- center
      } else {
        right <- center
      }
      center <- as.integer((left + right) / 2)
    }
    if (center == length(times))
      commits[[center]]
    else
      commits[[center+1]]
  }
}
