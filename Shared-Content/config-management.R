#' MARCS Configuration Management System
#'
#' This file provides functions for automatic user detection and configuration
#' loading, eliminating the need for manual user switching in global-config.R

#' Load User Configuration Automatically
#'
#' Automatically detects the current user and loads their configuration from config.yml.
#' Eliminates the need for manual commenting/uncommenting of user settings.
#'
#' @param config_file Path to the configuration YAML file (default: "config.yml")
#' @param user_override Optional string to override automatic user detection
#'
#' @return Named list containing user configuration with components:
#'   \describe{
#'     \item{local_sync_dir}{User's local synchronization directory}
#'     \item{repo_clone}{Path to repository clone}
#'     \item{initials}{User's initials for file naming}
#'     \item{platform}{User's platform (windows/mac/linux)}
#'     \item{studies}{List of available studies}
#'     \item{chrome_path}{Detected Chrome browser path}
#'   }
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Detects current system user automatically
#'   \item Loads configuration from YAML file
#'   \item Validates user configuration exists
#'   \item Sets up Chrome path based on platform
#'   \item Validates all paths exist and are accessible
#' }
#'
#' @examples
#' \dontrun{
#'   # Automatic user detection
#'   config <- load_user_config()
#'   
#'   # Override user detection
#'   config <- load_user_config(user_override = "andre")
#'   
#'   # Use configuration
#'   local_sync_dir <- config$local_sync_dir
#'   repo_clone <- config$repo_clone
#' }
#'
#' @export
load_user_config <- function(config_file = "config.yml", user_override = NULL) {
  
  # Load config package if not already loaded
  if (!require("config", quietly = TRUE)) {
    stop("config package is required. Install with: install.packages('config')")
  }
  
  # Auto-detect current user or use override
  if (is.null(user_override)) {
    current_user <- Sys.info()[["user"]]
  } else {
    current_user <- user_override
  }
  
  message(paste("Loading configuration for user:", current_user))
  
  # Load configuration file
  if (!file.exists(config_file)) {
    stop(paste("Configuration file not found:", config_file))
  }
  
  # Load default configuration and user-specific config
  default_config <- config::get(config = "default", file = config_file)
  
  # Try to load user-specific configuration
  user_config <- tryCatch({
    config::get(config = current_user, file = config_file)
  }, error = function(e) {
    stop(paste("User configuration not found for:", current_user, 
               "\nAvailable users:", paste(get_available_users(config_file), collapse = ", ")))
  })
  
  # Merge default and user configurations
  merged_config <- c(default_config, user_config)
  merged_config$current_user <- current_user
  
  # Detect and set Chrome path
  merged_config$chrome_path <- detect_chrome_path(merged_config)
  
  # Validate configuration
  validate_user_config(merged_config)
  
  # Set environment variable for Chrome
  Sys.setenv(CHROMOTE_CHROME = merged_config$chrome_path)
  
  message("Configuration loaded successfully")
  return(merged_config)
}

#' Get Available Users from Configuration
#'
#' Returns a list of available user configurations from the config file.
#'
#' @param config_file Path to the configuration YAML file
#'
#' @return Character vector of available user names
#'
#' @export
get_available_users <- function(config_file = "config.yml") {
  
  if (!require("yaml", quietly = TRUE)) {
    stop("yaml package is required. Install with: install.packages('yaml')")
  }
  
  if (!file.exists(config_file)) {
    stop(paste("Configuration file not found:", config_file))
  }
  
  # Read YAML file directly to get user names
  config_data <- yaml::read_yaml(config_file)
  
  if (!"users" %in% names(config_data)) {
    return(character(0))
  }
  
  return(names(config_data$users))
}

#' Detect Chrome Browser Path
#'
#' Automatically detects the Chrome browser path based on platform and configuration.
#'
#' @param config Configuration object containing platform and chrome_paths
#'
#' @return String path to Chrome browser executable
#'
#' @details
#' The function checks standard Chrome installation locations based on the user's platform.
#' If multiple Chrome installations are found, it returns the first valid one.
#'
#' @export
detect_chrome_path <- function(config) {
  
  # Determine platform if not specified
  if (is.null(config$platform)) {
    system_info <- Sys.info()
    platform <- tolower(system_info[["sysname"]])
    platform <- switch(platform,
                       "windows" = "windows",
                       "darwin" = "mac",
                       "linux" = "linux",
                       "windows")  # default fallback
  } else {
    platform <- config$platform
  }
  
  # Get default path for platform
  if (!is.null(config$chrome_paths) && platform %in% names(config$chrome_paths)) {
    default_path <- config$chrome_paths[[platform]]
    
    # Check if default path exists
    if (file.exists(default_path)) {
      return(default_path)
    }
  }
  
  # Alternative paths to search based on platform
  search_paths <- switch(platform,
    "windows" = c(
      "C:/Program Files/Google/Chrome/Application/chrome.exe",
      "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
      paste0(Sys.getenv("LOCALAPPDATA"), "/Google/Chrome/Application/chrome.exe")
    ),
    "mac" = c(
      "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
      "/Applications/Google Chrome Canary.app/Contents/MacOS/Google Chrome Canary"
    ),
    "linux" = c(
      "/usr/bin/google-chrome",
      "/usr/bin/google-chrome-stable",
      "/usr/bin/chromium-browser",
      "/snap/bin/chromium"
    )
  )
  
  # Search for Chrome in alternative paths
  for (path in search_paths) {
    if (file.exists(path)) {
      return(path)
    }
  }
  
  # If no Chrome found, issue warning but don't fail
  warning(paste("Chrome browser not found for platform:", platform, 
                "\nSome functionality may not work properly."))
  
  # Return default path even if it doesn't exist
  if (!is.null(config$chrome_paths) && platform %in% names(config$chrome_paths)) {
    return(config$chrome_paths[[platform]])
  }
  
  return("")
}

#' Validate User Configuration
#'
#' Validates that all required configuration settings are present and paths exist.
#'
#' @param config Configuration object to validate
#'
#' @return NULL (function called for side effects, throws errors on validation failure)
#'
#' @details
#' Validates the following configuration elements:
#' \itemize{
#'   \item local_sync_dir: Must exist and be accessible
#'   \item repo_clone: Must exist and be accessible
#'   \item initials: Must be present and non-empty
#'   \item chrome_path: Issues warning if Chrome not found
#' }
#'
#' @export
validate_user_config <- function(config) {
  
  # Check required fields
  required_fields <- c("local_sync_dir", "repo_clone", "initials")
  
  for (field in required_fields) {
    if (is.null(config[[field]]) || config[[field]] == "") {
      stop(paste("Required configuration field missing or empty:", field))
    }
  }
  
  # Validate directory paths exist
  if (!dir.exists(config$local_sync_dir)) {
    stop(paste("local_sync_dir does not exist:", config$local_sync_dir))
  }
  
  if (!dir.exists(config$repo_clone)) {
    stop(paste("repo_clone does not exist:", config$repo_clone))
  }
  
  # Check Chrome path exists (warning only, not fatal)
  if (!is.null(config$chrome_path) && config$chrome_path != "" && !file.exists(config$chrome_path)) {
    warning(paste("Chrome browser not found at:", config$chrome_path))
  }
  
  # Validate initials format (should be 2-3 characters)
  if (nchar(config$initials) < 2 || nchar(config$initials) > 3) {
    warning(paste("Initials should be 2-3 characters:", config$initials))
  }
  
  message("Configuration validation completed successfully")
  return(NULL)
}

#' Initialize Configuration System
#'
#' Convenience function to initialize the MARCS configuration system.
#' This replaces the manual configuration in global-config.R.
#'
#' @param config_file Path to configuration file (default: "config.yml")
#' @param user_override Optional user override for testing
#'
#' @return Named list with configuration and derived paths
#'
#' @examples
#' \dontrun{
#'   # Replace global-config.R initialization
#'   config <- initialize_marcs_config()
#'   
#'   # Access configuration
#'   local_sync_dir <- config$local_sync_dir
#'   repo_clone <- config$repo_clone
#'   initials <- config$initials
#' }
#'
#' @export
initialize_marcs_config <- function(config_file = "config.yml", user_override = NULL) {
  
  message("Initializing MARCS configuration system...")
  
  # Load user configuration
  config <- load_user_config(config_file, user_override)
  
  # Create derived paths (matching global-config.R structure)
  config$input_base_dir <- file.path(config$local_sync_dir, "Input")
  config$output_base_dir <- file.path(config$local_sync_dir, "Output")
  
  # Set global variables for backward compatibility
  assign("local_sync_dir", config$local_sync_dir, envir = .GlobalEnv)
  assign("repo_clone", config$repo_clone, envir = .GlobalEnv)
  assign("initials", config$initials, envir = .GlobalEnv)
  assign("studies", config$studies, envir = .GlobalEnv)
  assign("input_base_dir", config$input_base_dir, envir = .GlobalEnv)
  assign("output_base_dir", config$output_base_dir, envir = .GlobalEnv)
  
  message("MARCS configuration system initialized successfully")
  return(config)
}