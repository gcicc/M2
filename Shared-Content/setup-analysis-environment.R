#' Setup Analysis Environment for MARCS Validation Modules
#'
#' This function extracts and centralizes the common setup logic used across
#' all .qmd analysis files, reducing code duplication and ensuring consistent
#' environment configuration.
#'
#' @param params List of parameters from Quarto document containing:
#'   \itemize{
#'     \item study: Study identifier (character)
#'     \item repo_clone: Path to repository clone (character)  
#'     \item input_dir: Input directory path (character)
#'     \item output_dir: Output directory path (character)
#'     \item this_endpoint: Endpoint being analyzed (character)
#'   }
#' @param module_name String specifying the module name for function sourcing.
#'   Examples: "Accuracy", "Precision", "CE-VAST-Agreement", "association"
#'
#' @return Named list containing analysis context with components:
#'   \describe{
#'     \item{study}{Study identifier}
#'     \item{repo_clone}{Repository clone path}
#'     \item{input_dir}{Input directory path}
#'     \item{output_dir}{Output directory path}
#'     \item{this_endpoint}{Endpoint being analyzed}
#'     \item{table_path}{Directory path for saving tables}
#'     \item{figure_path}{Directory path for saving figures}
#'   }
#'
#' @details
#' This function performs the following setup tasks:
#' \itemize{
#'   \item Extracts parameters from Quarto params object
#'   \item Sources the shared preamble.R file with common libraries and functions
#'   \item Sources all R files from the specified module's functions directory
#'   \item Creates output directories for tables and figures
#'   \item Sets global variables for backward compatibility with existing code
#'   \item Returns context object for explicit parameter passing
#' }
#'
#' The function eliminates 15+ lines of repeated setup code across .qmd files
#' while maintaining full backward compatibility with existing analysis workflows.
#'
#' @examples
#' \dontrun{
#'   # In accuracy.qmd setup chunk:
#'   analysis_ctx <- setup_analysis_environment(params, "Accuracy")
#'   
#'   # In precision.qmd setup chunk:
#'   analysis_ctx <- setup_analysis_environment(params, "Precision")
#'   
#'   # For modules with nested structure:
#'   analysis_ctx <- setup_analysis_environment(params, "association", 
#'                     subdir = "Clinical-Biological-Validation")
#' }
#'
#' @seealso
#' \code{\link{source_module_functions}} for module-specific function loading
#'
#' @export
setup_analysis_environment <- function(params, module_name, subdir = NULL) {
  
  # Extract parameters into analysis context
  analysis_ctx <- list(
    study = params$study,
    repo_clone = params$repo_clone,
    input_dir = params$input_dir,
    output_dir = params$output_dir,
    this_endpoint = params$this_endpoint,
    table_path = file.path(params$output_dir, "tables"),
    figure_path = file.path(params$output_dir, "figures")
  )
  
  # Source shared preamble with common libraries and functions
  source(file.path(params$repo_clone, "Shared-Content", "preamble.R"))
  
  # Source module-specific functions
  source_module_functions(params$repo_clone, module_name, subdir)
  
  # Create output directories
  dir.create(analysis_ctx$table_path, showWarnings = FALSE, recursive = TRUE)
  dir.create(analysis_ctx$figure_path, showWarnings = FALSE, recursive = TRUE)
  
  # Set global variables for backward compatibility with existing code
  # This ensures existing .qmd files continue to work without modification
  assign("study", analysis_ctx$study, envir = .GlobalEnv)
  assign("repo_clone", analysis_ctx$repo_clone, envir = .GlobalEnv)
  assign("input_dir", analysis_ctx$input_dir, envir = .GlobalEnv)
  assign("output_dir", analysis_ctx$output_dir, envir = .GlobalEnv)
  assign("this_endpoint", analysis_ctx$this_endpoint, envir = .GlobalEnv)
  assign("table_path", analysis_ctx$table_path, envir = .GlobalEnv)
  assign("figure_path", analysis_ctx$figure_path, envir = .GlobalEnv)
  
  return(analysis_ctx)
}

#' Source Module-Specific Functions
#'
#' Helper function to source all R files from a module's functions directory.
#' Handles different module directory structures automatically.
#'
#' @param repo_clone Path to repository clone
#' @param module_name Module name (e.g., "Accuracy", "Precision")  
#' @param subdir Optional subdirectory for nested modules
#'
#' @return NULL (function called for side effects)
#'
#' @details
#' This function handles the different directory structures across modules:
#' \itemize{
#'   \item Analytical-Validation modules: repo/Analytical-Validation/MODULE/functions/
#'   \item CE-VAST-Agreement: repo/CE-VAST-Agreement/functions/
#'   \item Clinical-Biological: repo/Clinical-Biological-Validation/MODULE/functions/
#' }
#'
#' @examples
#' \dontrun{
#'   source_module_functions("/path/to/repo", "Accuracy")
#'   source_module_functions("/path/to/repo", "association", "Clinical-Biological-Validation")
#' }
#'
#' @export
source_module_functions <- function(repo_clone, module_name, subdir = NULL) {
  
  # Determine functions directory path based on module structure
  if (!is.null(subdir)) {
    # For nested modules like Clinical-Biological-Validation/association
    functions_path <- file.path(repo_clone, subdir, module_name, "functions")
  } else if (module_name %in% c("Accuracy", "Precision")) {
    # For Analytical-Validation modules
    functions_path <- file.path(repo_clone, "Analytical-Validation", module_name, "functions")
  } else if (module_name == "CE-VAST-Agreement") {
    # For CE-VAST-Agreement module
    functions_path <- file.path(repo_clone, "CE-VAST-Agreement", "functions")
  } else if (module_name == "association") {
    # For Clinical-Biological-Validation/association
    functions_path <- file.path(repo_clone, "Clinical-Biological-Validation", "association", "functions")
  } else {
    # Generic fallback - try module name as direct subdirectory
    functions_path <- file.path(repo_clone, module_name, "functions")
  }
  
  # Check if functions directory exists
  if (!dir.exists(functions_path)) {
    warning(paste("Functions directory not found:", functions_path))
    return(NULL)
  }
  
  # Find and source all R files in the functions directory
  r_files <- list.files(path = functions_path, pattern = "\\.R$", full.names = TRUE)
  cpp_files <- list.files(path = functions_path, pattern = "\\.cpp$", full.names = TRUE)
  
  total_files <- length(r_files) + length(cpp_files)
  if (total_files == 0) {
    warning(paste("No R or cpp files found in:", functions_path))
    return(NULL)
  }
  
  # Source each R file
  for (file in r_files) {
    source(file)
  }
  
  # Source each cpp file (if Rcpp is available)
  if (length(cpp_files) > 0) {
    if (requireNamespace("Rcpp", quietly = TRUE)) {
      for (file in cpp_files) {
        Rcpp::sourceCpp(file)
      }
    } else {
      warning("Rcpp package not available - skipping cpp files")
    }
  }
  
  # Report successful sourcing
  message(paste("Sourced", length(r_files), "R files and", length(cpp_files), "cpp files from", module_name, "module"))
  
  return(NULL)
}