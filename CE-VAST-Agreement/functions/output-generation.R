# Output generation for agreement relative to pathologist

#' Generate Kappa Executive Summary
#'
#' This function generates an executive summary table for Cohen's kappa agreement, summarizing the agreement between readers.
#' It processes the input data, formats the table, and saves it as an image.
#'
#' @param kappa.summary.in List of data frames containing kappa summary information.
#' @param reader_type The type of reader used, for use in filenames.
#' @return Formatted output table object.
#' @import dplyr
#' @import gt
#' @importFrom gt cols_merge cols_label cols_align fmt_number tab_header gtsave
generate_kappa_exec_summary <- function(kappa.summary.in, reader_type) {
  
  if (length(kappa.summary.in) == 0) {
    return(NULL)
  } else if (length(kappa.summary.in) == 1) {
    kappa.table.out <- kappa.summary.in[[1]] %>%
      dplyr::select(Pairing, Comparator, n, Estimate, LCL, UCL) %>%
      gt() %>%
      cols_merge(columns = c(Estimate, LCL, UCL), pattern = "{1} [{2}, {3}]") %>%
      cols_label(Estimate = "Estimate [95% CI]") %>%
      cols_align(align = "center", columns = everything()) %>%
      fmt_number(columns = c(Estimate, LCL, UCL), decimals = 2) %>%
      tab_header(
        title = paste("Cohen's kappa agreement for First 5%"),
        subtitle = paste0("Agreement with ", kappa.summary.in[[1]]$reader_type[1], " Reader")
      )
    
  } else if (length(kappa.summary.in) == 2) {
    kappa.summary.in[[1]]$`Image Quality` <- names(kappa.summary.in)[1]
    kappa.summary.in[[2]]$`Image Quality` <- names(kappa.summary.in)[2]
    
    kappa.table.out <- bind_rows(kappa.summary.in) %>%
      dplyr::select(Pairing, `Image Quality`, Comparator, n, Estimate, LCL, UCL) %>%
      gt() %>%
      cols_merge(columns = c(Estimate, LCL, UCL), pattern = "{1} [{2}, {3}]") %>%
      cols_label(Estimate = "Estimate [95% CI]") %>%
      cols_align(align = "center", columns = everything()) %>%
      fmt_number(columns = c(Estimate, LCL, UCL), decimals = 2) %>%
      tab_header(
        title = paste("Cohen's kappa agreement for First 5%"),
        subtitle = paste0("Agreement with ", kappa.summary.in[[1]]$reader_type[1], " Reader")
      )
  }
  
  # Save the table as an image
  gt::gtsave(kappa.table.out, filename = path(table_path, paste0("pathologist-agreement-", reader_type, ".png")))
  
  return(kappa.table.out)
}

#' Generate Kappa Figure
#'
#' This function generates a figure for Cohen's kappa agreement, summarizing the agreement between readers.
#' It processes the input data, creates the figure, and saves it as an image.
#'
#' @param kappa.summary.in List of data frames containing kappa summary information.
#' @param reader_type The type of reader used, for use in filenames.
#' @return Generated plot object.
#' @import dplyr
#' @import ggplot2
#' @importFrom ggplot2 geom_point geom_errorbarh labs ggsave
generate_kappa_fig <- function(kappa.summary.in, reader_type) {
  
  temp <- list()
  
  if (length(kappa.summary.in) == 0) {
    return(NULL)
    
  } else if (length(kappa.summary.in) == 1) {
    kappa.summary.in[[1]]$`Image Quality` <- ""
    temp <- kappa.summary.in[[1]]
    
  } else if (length(kappa.summary.in) == 2) {
    kappa.summary.in[[1]]$`Image Quality` <- names(kappa.summary.in)[1]
    kappa.summary.in[[2]]$`Image Quality` <- names(kappa.summary.in)[2]
    temp <- bind_rows(kappa.summary.in)
  }
  
  subtitle <- paste0("for ", reader_type, " reads")
  
  # Add a row combining Comparator and Image Quality for plotting
  temp$row <- paste(temp$Comparator, "-", temp$`Image Quality`)
  
  # Create the kappa figure
  fig.out <- temp %>%
    ggplot(aes(x = Estimate, xmin = LCL, xmax = UCL, y = row)) +
    geom_point() +
    geom_errorbarh() +
    labs(
      title = "Kappa Results by Comparator",
      subtitle = subtitle,
      x = "Kappa Estimate",
      y = "Pairing"
    ) +
    theme(aspect.ratio = 1) +
    scale_color_manual(values = cbp2)
  
  # Save the figure as an image
  ggsave(fig.out, filename = path(figure_path, paste0("pathologist-agreement-", reader_type, ".png")))
  
  return(fig.out)
}

#' Generate Kappa by Stratum
#'
#' This function generates a table summarizing Cohen's kappa agreement by stratum. 
#' It processes the input data, formats the table, and saves it as an image.
#'
#' @param kappa.summary.in List of data frames containing kappa summary information.
#' @param reader_type The type of reader used, for use in filenames.
#' @param stratifier The stratifier parameter for the analysis.
#' @return Formatted output table object.
#' @import dplyr
#' @import gt
#' @importFrom gt cols_merge cols_label cols_align fmt_number tab_header gtsave
generate_kappa_bystratum <- function(kappa.summary.in, reader_type, stratifier) {
  
  if (length(kappa.summary.in) == 0) {
    return(NULL)
    
  } else if (length(kappa.summary.in) == 1) {
    kappa.summary.in[[1]]$`Image Quality` <- ""
    kappa.table.out <- kappa.summary.in[[1]]
    
  } else if (length(kappa.summary.in) == 2) {
    kappa.summary.in[[1]]$`Image Quality` <- names(kappa.summary.in)[1]
    kappa.summary.in[[2]]$`Image Quality` <- names(kappa.summary.in)[2]
    kappa.table.out <- bind_rows(kappa.summary.in)
  }
  
  kappa.table.out <- kappa.table.out %>%
    group_by(Comparator, stratum) %>%
    dplyr::select(stratum, `Image Quality`, Comparator, n, Estimate, LCL, UCL) %>%
    gt() %>%
    cols_merge(columns = c(Estimate, LCL, UCL), pattern = "{1} [{2}, {3}]") %>%
    cols_label(Estimate = "Estimate [95% CI]") %>%
    cols_align(align = "center", columns = everything()) %>%
    fmt_number(columns = c(Estimate, LCL, UCL), decimals = 2) %>%
    tab_header(
      title = paste("Cohen's kappa agreement for First 5%"),
      subtitle = paste0("Agreement with ", kappa.summary.in[[1]]$reader_type[1], " Reader")
    )
  
  # Save the table as an image
  gt::gtsave(kappa.table.out, filename = path(table_path, paste0("pathologist-agreement-", reader_type, " by", stratifier, ".png")))
  
  return(kappa.table.out)
}

#' Generate Kappa Figure by Stratum
#'
#' This function generates a figure for Cohen's kappa agreement by stratum, summarizing the agreement between readers.
#' It processes the input data, creates the figure, and saves it as an image.
#'
#' @param kappa.summary.in List of data frames containing kappa summary information.
#' @param reader_type The type of reader used, for use in filenames.
#' @param stratifier The stratifier parameter for the analysis.
#' @return Generated plot object.
#' @import dplyr
#' @import ggplot2
#' @importFrom ggplot2 geom_point geom_errorbarh labs ggsave
generate_kappa_fig_bystratum <- function(kappa.summary.in, reader_type, stratifier) {
  
  temp <- list()
  
  if (length(kappa.summary.in) == 0) {
    return(NULL)
    
  } else if (length(kappa.summary.in) == 1) {
    kappa.summary.in[[1]]$`Image Quality` <- ""
    temp <- kappa.summary.in[[1]]
    
  } else if (length(kappa.summary.in) == 2) {
    kappa.summary.in[[1]]$`Image Quality` <- names(kappa.summary.in)[1]
    kappa.summary.in[[2]]$`Image Quality` <- names(kappa.summary.in)[2]
    temp <- bind_rows(kappa.summary.in)
  }
  
  subtitle <- paste0("for ", temp$reader_type[1], " reads")
  
  # Add a row combining Comparator and stratum for plotting
  temp$row <- paste(temp$Comparator, "-", temp$stratum)
  
  # Create the kappa figure
  fig.out <- temp %>%
    ggplot(aes(x = Estimate, xmin = LCL, xmax = UCL, y = row)) +
    geom_point() +
    geom_errorbarh() +
    labs(
      title = "Kappa Results by Comparator",
      subtitle = subtitle,
      x = "Kappa Estimate",
      y = "Pairing"
    ) +
    theme(aspect.ratio = 1) +
    scale_color_manual(values = cbp2)
  
  if(length(kappa.summary.in) == 2) fig.out <- fig.out + facet_wrap(~`Image Quality`)
  
  # Save the figure as an image
  ggsave(fig.out, filename = path(figure_path, paste0("pathologist-agreement-", reader_type, " by", stratifier, ".png")))
  
  return(fig.out)
}

#' Generate Kappa Relationship Plot
#'
#' This function generates relationship plots for Cohen's kappa agreement with the specified endpoint.
#' It processes the input data, creates the plots, and saves them as images.
#'
#' @param analysis.data.in_ext Data frame containing the input data (default: analysis.data.in_ext.ML[[1]]).
#' @param reader_type The type of reader used, for use in filenames.
#' @param this_endpoint The endpoint of interest in the analysis.
#' @return Generated plot object(s).
#' @import dplyr
#' @import ggplot2
#' @importFrom ggplot2 geom_jitter labs ggplot coord_cartesian ggsave
generate_kappa_relationship_plot <- function(analysis.data.in_ext = analysis.data.in_ext.ML[[1]], reader_type, this_endpoint) {
  
  # Determine the maximum value of the endpoint
  max_of_this_endpoint <- max(analysis.data.in_ext[[this_endpoint]])
  
  # Generate relationship plot for MO
  relationship.plot.MO <- analysis.data.in_ext %>%
    dplyr::select(-MO) %>%
    dplyr::rename(MO = MO.canonical) %>%
    dplyr::filter(!is.na(MO)) %>%
    ggplot(aes(x = !!sym(this_endpoint), y = MO, color = MO)) +
    coord_cartesian(xlim = c(0, max_of_this_endpoint)) +
    geom_jitter(height = 0.2) +
    theme(legend.position = "none", aspect.ratio = 1) +
    scale_color_manual(values = cbp2) +
    labs(title = paste(this_endpoint, "vs. Marsh-Oberhuber"), caption = paste0("for ", reader_type, " reads"))
  
  ggsave(plot = relationship.plot.MO, filename = path(figure_path, paste0("relationship.plot.MO-", reader_type, "-", this_endpoint, ".png")))
  
  # Check for the presence of HLA_G column and generate additional plots if present
  if (!("HLA_G" %in% colnames(analysis.data.in_ext))) {
    return(relationship.plot.MO)
  } else {
    relationship.plot.HLA_G <- analysis.data.in_ext %>%
      dplyr::select(-HLA_G) %>%
      dplyr::rename(HLA_G = HLA_G.canonical) %>%
      dplyr::filter(!is.na(HLA_G)) %>%
      ggplot(aes(x = !!sym(this_endpoint), y = HLA_G, color = HLA_G)) +
      coord_cartesian(xlim = c(0, max_of_this_endpoint)) +
      geom_jitter(height = 0.2) +
      theme(legend.position = "none", aspect.ratio = 1) +
      scale_color_manual(values = cbp2) +
      labs(title = paste(this_endpoint, "vs. HLA-G"), caption = paste0("for ", reader_type, " reads"))
    
    ggsave(plot = relationship.plot.HLA_G, filename = path(figure_path, paste0("relationship.plot.HLA_G-", reader_type, "-", this_endpoint, ".png")))
    
    relationship.plot.HLA_DQ <- analysis.data.in_ext %>%
      dplyr::select(-HLA_DQ) %>%
      dplyr::rename(HLA_DQ = HLA_DQ.canonical) %>%
      dplyr::filter(!is.na(HLA_DQ)) %>%
      ggplot(aes(x = !!sym(this_endpoint), y = HLA_DQ, color = HLA_DQ)) +
      coord_cartesian(xlim = c(0, max_of_this_endpoint)) +
      geom_jitter(height = 0.2) +
      theme(legend.position = "none", aspect.ratio = 1) +
      scale_color_manual(values = cbp2) +
      labs(title = paste(this_endpoint, "vs. HLA-DQ"), caption = paste0("for ", reader_type, " reads"))
    
    ggsave(plot = relationship.plot.HLA_DQ, filename = path(figure_path, paste0("relationship.plot.HLA_DQ-", reader_type, "-", this_endpoint, ".png")))
    
    # Create combined plot for all categories
    temp <- analysis.data.in_ext %>%
      dplyr::select(-MO, -HLA_G, -HLA_DQ) %>%
      dplyr::rename(MO = MO.canonical, HLA_G = HLA_G.canonical, HLA_DQ = HLA_DQ.canonical) %>%
      pivot_longer(cols = c(MO, HLA_G, HLA_DQ), names_to = "name", values_to = "value") %>%
      dplyr::filter(!is.na(value))
    
    temp$class <- factor(case_when(
      temp$value %in% c("M0-2", "M3A", "M3B", "M3C") ~ "MO",
      temp$value %in% c("HLA-g neg", "HLA-g pos") ~ "HLA_G",
      temp$value %in% c("DQ2 homoz", "DQ2 heteroz", "DQ2 and DQ8", "DQ8") ~ "HLA_DQ"
    ), levels = c("MO", "HLA_G", "HLA_DQ"))
    
    relationship.plot.all <- temp %>%
      ggplot(aes(x = !!sym(this_endpoint), y = value, color = class)) +
      coord_cartesian() +
      geom_jitter(height = 0.2) +
      theme(legend.position = "none") +
      scale_color_manual(values = cbp2) +
      scale_x_continuous(limits = c(0, max_of_this_endpoint)) +
      labs(title = paste(this_endpoint, "vs. Histological Phenotypes"), caption = paste0("for ", reader_type, " reads"), y = NULL) +
      facet_wrap(~class, scales = "free_y", ncol = 1, shrink = TRUE) +
      force_panelsizes(rows = c(2, 1, 2), cols = 1)
    
    ggsave(plot = relationship.plot.all, filename = path(figure_path, paste0("relationship.plot.combined-", reader_type, "-", this_endpoint, ".png")))
    
    return(relationship.plot.all)
  }
}
