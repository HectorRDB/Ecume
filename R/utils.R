# global reference to scipy (will be initialized in .onLoad)
sklrn <- NULL

.onLoad <- function(libname, pkgname) {
  # use superassignment to update global reference to sklrn
  sklrn <<- reticulate::import("sklearn.metrics", delay_load = TRUE)
}

#' Install the required python packages
#'
#' @description Install the required python packages
#'
#' @param method Passed to \link[reticulate]{py_install}
#' @param conda Passed to \link[reticulate]{py_install}
#' @examples
#' \dontrun{
#'  install_depencencis()
#' }
#' @details
#' This install the required python packages for the installation.
#' @return
#' Nothing
#' @importFrom reticulate py_install
#' @export
install_dependencies <- function(method = "auto", conda = "auto") {
  reticulate::py_install("sklearn", method = method, conda = conda)
}
