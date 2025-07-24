# MARCS Validation Analysis - Implementation Roadmap

This document organizes the code improvement recommendations discovered during the comprehensive analysis of the MARCS validation codebase. The suggestions are grouped by implementation complexity and impact, designed for incremental git commits.

## Overview

The analysis identified key areas for improvement across the entire analytical pipeline:
- Configuration management and user experience
- Code organization and modularity  
- Component architecture and coupling
- Report harmonization across modules
- ETL system efficiency and maintainability

## Critical Implementation Notes

### Systematic Module Coverage
This roadmap focuses on improvements identified through detailed analysis of the **Accuracy** module. The same patterns and issues exist across **all analysis modules**:

- **Analytical-Validation/**: Precision
- **CE-VAST-Agreement/**: Gastroenterologist Agreement, Pathologist Agreement  
- **Clinical-Biological-Validation/**: Single Timepoint Association, Longitudinal Association, Proportion Explained
- **Clinical-Utility-and-Use/**: Clinical Trial Utility

**Each improvement must be systematically applied across all modules** following the same patterns identified in the Accuracy analysis.

### Output Validation Requirements
**CRITICAL**: Before any git commit, all changes must pass output equivalence testing:

1. **Baseline Generation**: Run existing code to generate reference outputs
2. **Modified Code Testing**: Run updated code with identical inputs  
3. **Bitwise Comparison**: Verify all generated files are identical
4. **Report Validation**: Confirm PDF reports have identical content
5. **Data Structure Testing**: Validate all intermediate R objects match exactly

**No code changes should be committed without 100% output equivalence verification.**

## Quick Wins (Low Risk, High Impact)

### QW1: Standardize Documentation
**Priority: High | Effort: 1-2 days | Risk: Very Low**

**Problem**: Inconsistent function documentation across modules
- `analysis-block.R` functions lack proper roxygen2 documentation
- Missing `@param`, `@return`, and `@examples` specifications
- No standardized documentation template

**Solution**:
```r
#' Perform Main Analysis for [Module Name]
#' 
#' @param analysis_data Data frame with required columns: STUDYID, SITE, SUBJID, READER, endpoint
#' @param endpoint String specifying which endpoint to analyze (e.g., "BVA", "First5Percent_Mean")
#' @param study String specifying study name for study-specific processing
#' @return Named list with components:
#'   \item{results}{Analysis results data frame}
#'   \item{plots}{List of ggplot objects}
#'   \item{tables}{List of summary tables}
#' @examples
#' \dontrun{
#'   results <- main_analysis(data, "BVA", "TAK-062-2001")
#' }
```

**Implementation Steps**:
1. Create documentation template in `docs/function-template.R`
2. **Apply to ALL modules systematically**:
   - Start with `Analytical-Validation/Accuracy/` (reference implementation)
   - Apply same patterns to `Analytical-Validation/Precision/`
   - Extend to `CE-VAST-Agreement/` functions
   - Complete with `Clinical-Biological-Validation/` modules
3. Update all `main_analysis()` functions with proper roxygen2
4. Add documentation to output generation functions
5. Generate documentation with `devtools::document()`

**Testing Requirements**:
- Generate baseline documentation before changes
- Verify all functions are properly documented after changes
- Confirm no functional changes occurred during documentation updates

**Files to Modify** (Complete Module List):
- `Analytical-Validation/Accuracy/functions/analysis-block.R`
- `Analytical-Validation/Precision/functions/analysis-block.R`
- `CE-VAST-Agreement/functions/analysis-block.R`
- `Clinical-Biological-Validation/association/functions/analysis-block.R`
- All corresponding `output-generation.R` files across modules

---

### QW2: Extract Common Setup Logic
**Priority: High | Effort: 1 day | Risk: Very Low**

**Problem**: 15-line setup chunk repeated identically across 7+ .qmd files

**Current Pattern**:
```r
# Repeated in every .qmd analysis chunk
study <- params$study
repo_clone <- params$repo_clone
input_dir <- params$input_dir
output_dir <- params$output_dir
this_endpoint <- params$this_endpoint

source(paste0(repo_clone, "/Shared-Content/preamble.R"))
functions_path <- path(repo_clone, "MODULE_NAME", "functions")
r_files <- list.files(path=functions_path, pattern="\\.R$", full.names=TRUE)
for (file in r_files) {
  source(file)
}
table_path <- path(output_dir, "tables")
dir.create(table_path)
figure_path <- path(output_dir, "figures")
dir.create(figure_path)
```

**Solution**: Create `Shared-Content/setup-analysis-environment.R`
```r
setup_analysis_environment <- function(params, module_name) {
  # Extract parameters
  analysis_ctx <- list(
    study = params$study,
    repo_clone = params$repo_clone,
    input_dir = params$input_dir,
    output_dir = params$output_dir,
    this_endpoint = params$this_endpoint,
    table_path = file.path(params$output_dir, "tables"),
    figure_path = file.path(params$output_dir, "figures")
  )
  
  # Setup environment
  source(file.path(params$repo_clone, "Shared-Content", "preamble.R"))
  source_module_functions(params$repo_clone, module_name)
  
  # Create output directories
  dir.create(analysis_ctx$table_path, showWarnings = FALSE, recursive = TRUE)
  dir.create(analysis_ctx$figure_path, showWarnings = FALSE, recursive = TRUE)
  
  # Set global variables for backward compatibility
  list2env(analysis_ctx, envir = .GlobalEnv)
  
  return(analysis_ctx)
}
```

**Updated .qmd chunks** (reduce from 15 lines to 2):
```r
{r setup}
analysis_ctx <- setup_analysis_environment(params, "Accuracy")  # Module name varies
```

**Systematic Application Across Modules**:
1. **Accuracy**: `setup_analysis_environment(params, "Accuracy")`
2. **Precision**: `setup_analysis_environment(params, "Precision")`  
3. **CE-VAST-Agreement**: `setup_analysis_environment(params, "CE-VAST-Agreement")`
4. **Clinical-Bio modules**: Module-specific names for each analysis type

**Output Validation Protocol**:
- Generate test reports from each module before/after changes
- Compare PDF outputs byte-for-byte using `tools::md5sum()`
- Verify all intermediate R objects (tables, plots) are identical
- Test with multiple studies (TAK-062-2001, Milan, Sheffield) per module

---

### QW3: Create Shared YAML Templates
**Priority: Medium | Effort: 1 day | Risk: Very Low**

**Problem**: Inconsistent YAML headers across .qmd files
- 2 different PDF formatting approaches
- Parameter mismatches (6 vs 4 parameters)
- Repeated header configuration

**Solution**: Create `shared-templates/`
```yaml
# shared-templates/standard-header.yml
format: 
  pdf:
    header-includes: 
      \usepackage{geometry}
      \geometry{top=0.75in,left=0.80in,bottom=0.5in,right=0.80in}
    include-in-header: 
      text: |
        \usepackage{scrlayer-scrpage}
        \cohead[{\includegraphics[height=4cm]{images/Logo.png}}]{MARCS Analysis Report}
    embed-resources: true
    smooth-scroll: true
    toc: true
    toc-location: left
    number-sections: true

# shared-templates/standard-params.yml  
params:
  analysis_type: "formal type of this analysis"
  study: "a study"
  repo_clone: "directory where the clone is"
  input_dir: "directory for the input"
  output_dir: "directory for the output"
  this_endpoint: "which of the MARCS scores to use"
```

**Usage in .qmd files**:
```yaml
---
title: "**Accuracy of MARCS {{params.this_endpoint}}**"
author: "MARCS Crossfunctional Team"
date: last-modified
format: !include ../shared-templates/standard-header.yml
params: !include ../shared-templates/standard-params.yml
---
```

---

## Moderate Changes (Medium Risk, High Impact)

### MC1: Implement Configuration-Based User Management
**Priority: High | Effort: 3-5 days | Risk: Medium**

**Problem**: Manual user switching in `global-config.R`
- Requires commenting/uncommenting code blocks for different users
- Hardcoded paths and settings
- No validation of user environment

**Solution**: Create `config.yml` and auto-detection system

**Implementation**:
1. Create `config.yml`:
```yaml
default:
  studies: ["Mock-Study", "Sheffield", "Milan", "Phase0", "TAK-062-2001"]
  chrome_paths:
    windows: "C:/Program Files/Google/Chrome/Application/chrome.exe"
    mac: "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"

users:
  eri7441:
    local_sync_dir: "C:/Users/eri7441/OneDrive - Takeda/Documents - MARCS/DA"
    repo_clone: "C:/ForGit/MARCS"
    initials: "GC"
  andre:
    local_sync_dir: "C:/Users/andre/MARCS-Validation-Data"
    repo_clone: "C:/BBMSC_ROOT/home/GitHub/Takeda/MARCS"
    initials: "AB"
```

2. Replace `global-config.R` with:
```r
library(config)

# Auto-detect user and load configuration
current_user <- Sys.info()[["user"]]
config <- config::get(config = current_user, file = "config.yml")

# Validate configuration
validate_user_config(config)

# Set environment variables
local_sync_dir <- config$local_sync_dir
repo_clone <- config$repo_clone
initials <- config$initials
Sys.setenv(CHROMOTE_CHROME = detect_chrome_path())
```

3. Add validation function:
```r
validate_user_config <- function(config) {
  stopifnot("local_sync_dir must exist" = dir.exists(config$local_sync_dir))
  stopifnot("repo_clone must exist" = dir.exists(config$repo_clone))
  chrome_path <- detect_chrome_path()
  stopifnot("Chrome not found" = file.exists(chrome_path))
}
```

---

### MC2: Modernize Analysis Pipeline with Purrr
**Priority: Medium | Effort: 2-3 days | Risk: Medium**

**Problem**: Manual loops and list building in .qmd files
- Imperative programming style with manual indexing
- Complex nested data access patterns
- Error-prone list construction

**Current Code** (from accuracy.qmd):
```r
results.ML <- list()
for (i in 1:length(form_merged_data_sets_output$merged_data_sets)) {
  merged_data_set <- form_merged_data_sets_output$merged_data_sets[[i]]
  label.name <- names(form_merged_data_sets_output$merged_data_sets)[i]
  
  analysis.data.in.ML <- merged_data_set$merged_data.ML$merged_data.ML.full %>% mutate(label.name=label.name)
  analysis.data.in.Human <- merged_data_set$merged_data.Human$merged_data.Human.full %>% mutate(label.name=label.name)
  analysis.data.in <- bind_rows(analysis.data.in.ML, analysis.data.in.Human)
  
  results <- main_analysis(analysis.data.in=analysis.data.in, this_endpoint=this_endpoint)
  results.ML[[length(results.ML)+1]] <- results
  names(results.ML)[length(results.ML)] <- label.name
}
```

**Improved with Purrr**:
```r
# Create helper function
prepare_combined_data <- function(merged_data_set, label.name) {
  list(
    ML = merged_data_set$merged_data.ML$merged_data.ML.full %>% mutate(label.name = label.name),
    Human = merged_data_set$merged_data.Human$merged_data.Human.full %>% mutate(label.name = label.name)
  ) %>%
  bind_rows()
}

# Functional pipeline
results.ML <- form_merged_data_sets_output$merged_data_sets %>%
  imap(~ prepare_combined_data(.x, .y)) %>%
  map(~ main_analysis(.x, this_endpoint)) %>%
  set_names(names(form_merged_data_sets_output$merged_data_sets))
```

**Implementation Steps**:
1. Extract data preparation functions to `Shared-Content/data-helpers.R`
2. Replace manual loops in accuracy.qmd with purrr pipeline
3. Test with existing data to ensure identical outputs
4. Apply pattern to precision.qmd and other modules
5. Update documentation with functional programming examples

---

### MC3: Separate Output Generation from File Saving
**Priority: Medium | Effort: 2-3 days | Risk: Medium**

**Problem**: Mixed responsibilities in output functions
- Functions both create objects AND save files to disk
- Global variable dependencies (`table_path`, `figure_path`)
- Difficult to test and debug

**Current Pattern**:
```r
deming_plot <- function(deming.fit) {
  # Generate plot
  plot <- create_plot_object(deming.fit)
  
  # Save to disk (side effect)
  full_file_path <- file.path(figure_path, filename)  # Global dependency!
  ggsave(filename = full_file_path, plot = plot)
  
  return(plot)
}
```

**Improved Pattern**:
```r
# Pure generation functions
generate_deming_plot <- function(deming_fit) {
  # Only creates and returns plot object
  # No side effects, fully testable
}

# Separate saving functions  
save_plot <- function(plot, output_ctx, filename) {
  full_path <- file.path(output_ctx$figure_path, filename)
  ggsave(filename = full_path, plot = plot)
}

# Composition in .qmd
plot <- generate_deming_plot(results.ML)
save_plot(plot, analysis_ctx, "deming_plot.png")
plot  # Display in report
```

---

## Large Changes (High Risk, High Impact)

### LC1: Refactor ETL System with Configuration-Driven Architecture
**Priority: High | Effort: 1-2 weeks | Risk: High**

**Problem**: Massive code duplication in ETL system
- 20 prep-data files with ~80% identical code
- 1,600+ lines of repetitive ETL logic
- Manual study branching in `form-merged-data-sets.R`

**Current Issues**:
- Adding new study requires creating 4 new R files
- Schema changes must be replicated across multiple files
- No centralized configuration for study-specific logic

**Proposed Architecture**:
```
Shared-Content/
├── etl-core/
│   ├── etl-registry.R          # Dynamic function registration
│   ├── transformation-lib.R     # Reusable transformations  
│   ├── validation-lib.R         # Schema validation
│   └── etl-templates.R         # Template generators
├── study-configs/
│   ├── study-registry.yml      # Central study configuration
│   ├── tak-062-2001.yml        # Study-specific config
│   ├── milan.yml               # Study-specific config
│   └── sheffield.yml           # Study-specific config
└── legacy-prep-data/           # Move existing files here
    └── prep-data-*.R           # Keep for reference/migration
```

**Implementation Plan**:

**Phase 1: Create Core Infrastructure (Week 1)**
1. Create `etl-core/etl-templates.R`:
```r
create_demog_etl <- function(study_config) {
  function(provenance, log_dir = NULL) {
    validate_input_dir(study_config$expected_pattern)
    raw_data <- load_data_file(study_config$file_pattern)
    
    standardized_data <- raw_data %>%
      apply_column_mapping(study_config$column_mapping) %>%
      apply_transformations(study_config$transformations) %>%
      standardize_factors(study_config$factor_levels)
    
    return(create_etl_output(standardized_data, study_config))
  }
}
```

2. Create study configuration files:
```yaml
# study-configs/tak-062-2001.yml
study_id: "TAK-062-2001"
demog:
  file_pattern: "Demographics_data - unblinded.xlsx"
  column_mapping:
    "Study Identifier": "STUDYID"
    "Study Site Identifier": "SITEID"
    "Subject Identifier for the Study": "SUBJID"
  transformations:
    - type: "factor_recode"
      column: "ARM"
      from: ["C1PGS", "C1T600GS"]
      to: ["Placebo", "TAK-062"]
  validation:
    required_columns: ["STUDYID", "SITE", "SUBJID", "ARM"]
```

**Phase 2: Migration and Testing (Week 2)**
3. Create migration script to generate configs from existing prep-data files
4. Implement registry system to replace manual branching
5. Test with one study (TAK-062-2001) to ensure identical outputs
6. Gradually migrate other studies
7. Update `form-merged-data-sets.R` to use registry

**Expected Benefits**:
- **87% code reduction** (1,600 lines → 200 lines + configs)
- **Single YAML file** to add new studies
- **Automatic validation** and consistent error handling
- **Unit testable** template functions

**Risk Mitigation**:
- Keep all existing prep-data files as backup
- Implement comprehensive testing for output equivalence  
- Migrate one study at a time
- Maintain backward compatibility during transition

---

### LC2: Implement Component-Based Architecture
**Priority: Medium | Effort: 2-3 weeks | Risk: High**

**Problem**: Tight coupling between analysis components
- Monolithic analysis functions with study-specific branching
- Global variable dependencies
- Mixed responsibilities (analysis + output generation)

**Current Issues**:
- 200-line `main_analysis()` functions with embedded study logic
- Functions can't be tested independently
- Difficult to reuse components across analysis types

**Proposed Architecture**:
```r
# Strategy pattern for study-specific processing
create_study_processor <- function(study) {
  processors <- list(
    Milan = milan_processor(),
    Sheffield = sheffield_processor(),
    "TAK-062-2001" = tak_processor()
  )
  processors[[study]]
}

# Modular analysis pipeline
main_analysis <- function(data, endpoint, study) {
  processor <- create_study_processor(study)
  
  processed_data <- processor$process(data, endpoint)
  analysis_results <- run_core_analysis(processed_data)
  formatted_results <- format_results(analysis_results, processor$config)
  
  return(formatted_results)
}

# Dependency injection for output paths
create_output_context <- function(output_dir) {
  list(
    table_path = file.path(output_dir, "tables"),
    figure_path = file.path(output_dir, "figures"),
    ensure_dirs = function() {
      dir.create(self$table_path, showWarnings = FALSE, recursive = TRUE)
      dir.create(self$figure_path, showWarnings = FALSE, recursive = TRUE)
    }
  )
}
```

**Implementation Phases**:

**Phase 1: Extract Study-Specific Logic**
1. Identify study-specific code patterns in analysis-block.R files
2. Create study processor classes
3. Extract common analysis logic into reusable functions

**Phase 2: Implement Dependency Injection**
1. Replace global variables with context objects
2. Update output functions to accept context parameters
3. Modify .qmd files to pass context explicitly

**Phase 3: Create Modular Pipeline**
1. Break down monolithic analysis functions
2. Implement composable analysis steps
3. Add comprehensive error handling and logging

---

### LC3: Interactive Analysis Launcher
**Priority: Low | Effort: 1 week | Risk: Medium**

**Problem**: Manual configuration in `run-one-off.R`
- Requires commenting/uncommenting analysis type selections
- No interactive workflow for programmers
- Error-prone manual configuration

**Solution**: Create interactive CLI tool

```r
# run-analysis.R
library(cli)

main <- function() {
  cli_h1("MARCS Validation Analysis Launcher")
  
  # Interactive selection or command line args
  args <- parse_command_line()
  
  if (is.null(args$analysis_type)) {
    analysis_type <- select_analysis_type()
  } else {
    analysis_type <- args$analysis_type
  }
  
  if (is.null(args$study)) {
    study <- select_study()
  } else {
    study <- args$study
  }
  
  if (is.null(args$endpoint)) {
    endpoint <- select_endpoint()
  } else {
    endpoint <- args$endpoint
  }
  
  # Validate configuration
  validate_analysis_config(analysis_type, study, endpoint)
  
  # Run analysis
  cli_alert_info("Starting {analysis_type} analysis for {study} on {endpoint}")
  run_analysis_pipeline(analysis_type, study, endpoint)
}

select_analysis_type <- function() {
  choices <- c("Accuracy", "Precision", "Gastroenterologist Agreement", 
               "Path Agreement", "Single Timepoint Association")
  menu(choices, title = "Select analysis type:")
}
```

**Usage**:
```bash
# Interactive mode
Rscript run-analysis.R

# Command line mode  
Rscript run-analysis.R --analysis=Accuracy --study=TAK-062-2001 --endpoint=BVA
```

---

## Implementation Timeline

### Week 1-2: Quick Wins
- [ ] **QW1**: Standardize documentation across all modules
- [ ] **QW2**: Extract common setup logic into shared functions
- [ ] **QW3**: Create shared YAML templates for consistent formatting

### Week 3-4: Moderate Changes (Phase 1)
- [ ] **MC1**: Implement configuration-based user management  
- [ ] **MC2**: Modernize analysis pipeline with purrr
- [ ] **MC3**: Separate output generation from file saving

### Week 5-6: Large Changes (Phase 1)
- [ ] **LC1**: Begin ETL system refactoring
  - Create core infrastructure and templates
  - Implement configuration system
  - Migrate one study for testing

### Week 7-8: Large Changes (Phase 2)  
- [ ] **LC1**: Complete ETL system migration
  - Migrate remaining studies
  - Update form-merged-data-sets.R
  - Comprehensive testing and validation

### Week 9-10: Advanced Features
- [ ] **LC2**: Implement component-based architecture
- [ ] **LC3**: Create interactive analysis launcher

## Risk Assessment

### Low Risk Changes
- Documentation updates (QW1)
- Shared template creation (QW3)
- Function extraction (QW2)

### Medium Risk Changes  
- Configuration system (MC1) - Test thoroughly with existing workflows
- Purrr refactoring (MC2) - Validate output equivalence
- Output separation (MC3) - Ensure backward compatibility

### High Risk Changes
- ETL system overhaul (LC1) - Major architecture change, requires careful migration
- Component architecture (LC2) - Significant refactoring with potential breaking changes

## Success Metrics

### Code Quality
- [ ] Reduce code duplication by >80%
- [ ] Achieve >95% function documentation coverage
- [ ] Implement unit tests for all shared functions

### Developer Experience
- [ ] Eliminate manual user configuration switching
- [ ] Reduce .qmd setup code from 15 lines to 2 lines
- [ ] Enable new study addition with single YAML file

### Maintainability  
- [ ] Centralize configuration management
- [ ] Implement consistent error handling across modules
- [ ] Create reusable component library

## Comprehensive Testing Strategy

### Output Equivalence Testing Framework

**Create**: `tests/output-validation.R`
```r
validate_module_outputs <- function(module_name, studies = c("TAK-062-2001", "Milan")) {
  baseline_dir <- file.path("tests/baselines", module_name)
  test_dir <- file.path("tests/current", module_name)
  
  for (study in studies) {
    # Run analysis with baseline and current code
    baseline_results <- run_analysis_baseline(module_name, study)
    current_results <- run_analysis_current(module_name, study)
    
    # Compare all outputs
    compare_pdf_reports(baseline_results$pdf, current_results$pdf)
    compare_data_objects(baseline_results$data, current_results$data)
    compare_plot_objects(baseline_results$plots, current_results$plots)
    compare_table_objects(baseline_results$tables, current_results$tables)
  }
}
```

### Module-by-Module Testing Protocol

**For Each Implementation Step**:

1. **Pre-Change Baseline Generation**:
   ```r
   # Generate baselines for ALL modules before any changes
   modules <- c("Accuracy", "Precision", "Gastro-Agreement", "Path-Agreement", 
                "Single-Timepoint-Association", "Longitudinal-Association")
   
   for (module in modules) {
     generate_baseline_outputs(module, all_studies)
   }
   ```

2. **Systematic Module Updates**:
   - Implement change in **Accuracy** module first (reference implementation)
   - Validate Accuracy outputs match baseline exactly
   - Apply identical pattern to **Precision** module
   - Validate Precision outputs match baseline exactly  
   - Continue systematically through all modules
   - **Never proceed to next module until current module validates 100%**

3. **Cross-Module Integration Testing**:
   ```r
   # After all modules updated, test cross-module compatibility
   test_analysis_pipeline_integration()
   test_shared_function_compatibility()
   test_config_system_across_modules()
   ```

### Automated Validation Suite

**Create**: `tests/run-full-validation.R`
```r
run_full_validation_suite <- function() {
  cat("Starting comprehensive validation...\n")
  
  # Test each module individually
  modules <- get_all_analysis_modules()
  for (module in modules) {
    cat(sprintf("Validating %s module...\n", module))
    result <- validate_module_outputs(module)
    if (!result$success) {
      stop(sprintf("Module %s failed validation: %s", module, result$error))
    }
  }
  
  # Test integration
  cat("Testing cross-module integration...\n")
  integration_result <- test_integration()
  
  # Test configuration system
  cat("Validating configuration system...\n")
  config_result <- test_configuration_system()
  
  cat("All validation tests PASSED ✓\n")
}
```

### Git Commit Safety Protocol

**Before ANY commit**:
1. Run `tests/run-full-validation.R`
2. Confirm all modules produce identical outputs
3. Verify no regression in processing time
4. Check that all required files are still generated
5. Test with different user configurations

**Commit Message Template**:
```
[Module] Brief description of change

- Updated: List of specific files modified
- Modules: List of modules affected  
- Validation: Confirmed identical outputs for all modules
- Studies tested: TAK-062-2001, Milan, Sheffield, Phase0

✓ Output validation PASSED
✓ Integration tests PASSED  
✓ Performance regression check PASSED
```

### Rollback Procedures
- Keep original files in `legacy/` directories during transition
- Maintain branch checkpoints before major changes
- Document exact steps to revert each modification
- Test rollback procedures before implementing changes

### Migration Safety
- **Never modify more than one module at a time**
- **Always validate current module before proceeding to next**
- Maintain all original files during transition
- Implement comprehensive test suite for critical workflows
- Document all breaking changes and migration steps

---

*This roadmap provides a structured approach to modernizing the MARCS validation analysis codebase while maintaining output compatibility and minimizing disruption to current workflows. Each change is designed to be implemented incrementally with proper testing and validation.*