#' Source files recursively
#'
#' Modification of \code{\link[R.utils]{sourceDirectory}} which repairs the
#' behaviour of the ... argument, so that \code{chdir} and \code{local} can be
#' passed to \code{\link[R.utils]{sourceTo}}.
source_directory <-  function(
  path, pattern = ".*[.](r|R|s|S|q)([.](lnk|LNK))*$", recursive = TRUE,
  envir = parent.frame(), onError = c("error", "warning", "skip"),
  modifiedOnly = FALSE, ..., verbose = FALSE
) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'path':
  path <- R.utils::filePath(path)
  if (!R.utils::isDirectory(path))
    return(NULL)
  
  # Argument 'onError'
  onError <- match.arg(onError)
  
  # Argument 'verbose'
  verbose <- R.utils::Arguments$getVerbose(verbose)
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # start...
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Store files that get sourced.
  sourcedFiles <- c()
  
  # First, if recursive, follow all directories...
  if (recursive) {
    verbose && R.utils::cat(verbose, "Sourcing directory recursively: ", path)
    dirs <- list.files(path=path, recursive=FALSE,
                       all.files=TRUE, full.names=TRUE)
    dirs <- dirs[!(basename(dirs) %in% c(".", ".."))]
    
    # Source directories in lexicographic order
    if (length(dirs) > 0)  # To avoid warning():s
      dirs <- sort(dirs)
    
    for (dir in dirs) {
      pathname <- R.utils::filePath(dir)
      if (R.utils::isDirectory(pathname)) {
        verbose && R.utils::cat(verbose, "Entering: ", pathname)
        sourcedFiles <- c(
          sourcedFiles,
          source_directory(
            pathname, pattern=pattern, recursive=recursive, envir=envir,
            onError=onError, verbose=verbose, modifiedOnly = modifiedOnly,  ...
          )
        )
        
      }
    } # for (dir ...)
  } else {
    verbose && R.utils::cat(verbose, "Sourcing directory (non-recursively): ", path)
  }
  
  # Then, get all files in current directory...
  files <- R.utils::listDirectory(path, pattern=pattern, recursive=FALSE,
                                  allNames=TRUE, fullNames=TRUE)
  
  # Source files in lexicographic order
  if (length(files) > 0)  # To avoid warning():s
    files <- sort(files)
  
  if (verbose) {
    if (length(files) > 0) {
      R.utils::cat(verbose, "Found *.R scripts:")
      readable <- (sapply(files, FUN=file.access, mode=4) == 0)
      bytes <- sapply(files, FUN=function(x) file.info(x)$size)
      df <- data.frame(filename=basename(files), bytes=bytes,
                       readable=readable, row.names=NULL)
      print(verbose, df)
      # Not needed anymore
      df <- bytes <- readable <- NULL
    } else {
      R.utils::cat(verbose, "Found no *.R scripts.")
    }
  }
  
  for (file in files) {
    pathname <- R.utils::filePath(file)
    if (!R.utils::isDirectory(pathname)) {
      # If the parent directory is called 'global' then source to
      # the global environment, otherwise the local job environment.
      parent <- basename(dirname(pathname))
      local <- (parent != "global")
      type <- ifelse(local, "local", "global")
      
      tryCatch({
        verbose && enter(verbose, "Loading (", type, ") source file: ",
                         basename(pathname))
        
        dots <- list(...)
        if (! "chdir" %in% names(dots)) {
          chdir <- FALSE
        } else {
          chdir <- dots$chdir
          dots$chdir <- NULL
        }
        if (! "local" %in% names(dots)) {
          local <- TRUE
        } else {
          local <- dots$local
          dots$local <- NULL
        }
        
        do.call(
          what = R.utils::sourceTo,
          args = c(
            list(
              file = pathname,
              chdir = chdir,
              local = local,
              envir = envir,
              modifiedOnly = modifiedOnly
            ),
            dots
          )
        )
        
        sourcedFiles <- c(sourcedFiles, pathname)
        
        verbose && exit(verbose)
      }, error = function(ex) {
        if (verbose) {
          print(verbose, ex)
          tryCatch({
            # Display source code with erroneous line highlighted.
            R.utils::cat(verbose, R.utils::displayCode(pathname, highlight=ex$message,
                                                       pager="none"))
          }, error = function(ex) {})
        }
        verbose && exit(verbose, suffix="...failed")
        
        # An error was detected, but always log it.
        verbose && R.utils::cat(verbose, "Error when sourcing file ", pathname, ": ",
                                ex$message)
        
        if (onError == "skip") {
          # Ignore the error, but log it.
        } else if (onError == "warning") {
          # Give a warning.
          warning(ex$message)
        } else {
          # Rethrow error.
          signalCondition(ex)
          msg <- sprintf("source_directory() failed to source '%s': %s",
                         pathname, ex$message)
          stop(msg)
        }
      }) # tryCatch()
    }
  } # for (file ...)
  
  # Return files that was sourced.
  invisible(sourcedFiles)
}
