# Test script for the new MARCS configuration system
# This script demonstrates the new automated configuration loading

# Load the configuration management functions
source("Shared-Content/config-management.R")

cat("=== MARCS Configuration System Test ===\n\n")

# Test 1: Check available users
cat("Available users in config.yml:\n")
available_users <- get_available_users()
for (user in available_users) {
  cat(paste("-", user, "\n"))
}
cat("\n")

# Test 2: Auto-detect current user
current_user <- Sys.info()[["user"]]
cat(paste("Current system user:", current_user, "\n"))

# Test 3: Load configuration for current user
cat("\nTesting configuration loading...\n")
tryCatch({
  config <- load_user_config()
  
  cat("Configuration loaded successfully!\n")
  cat(paste("- User:", config$current_user, "\n"))
  cat(paste("- Initials:", config$initials, "\n"))
  cat(paste("- Local sync dir:", config$local_sync_dir, "\n"))
  cat(paste("- Repo clone:", config$repo_clone, "\n"))
  cat(paste("- Chrome path:", config$chrome_path, "\n"))
  cat(paste("- Platform:", config$platform, "\n"))
  cat("- Available studies:", paste(config$studies, collapse = ", "), "\n")
  
}, error = function(e) {
  cat(paste("ERROR:", e$message, "\n"))
  cat("\nThis is expected if you're not one of the configured users.\n")
  cat("To add your user, update config.yml with your settings.\n")
})

# Test 4: Test the full initialization
cat("\n=== Testing full MARCS initialization ===\n")
tryCatch({
  marcs_config <- initialize_marcs_config()
  cat("Full MARCS configuration initialized successfully!\n")
  
  # Verify global variables were set
  if (exists("local_sync_dir", envir = .GlobalEnv)) {
    cat("✓ Global variables set correctly\n")
  } else {
    cat("✗ Global variables not set\n")
  }
  
}, error = function(e) {
  cat(paste("ERROR in full initialization:", e$message, "\n"))
})

cat("\n=== Test Complete ===\n")