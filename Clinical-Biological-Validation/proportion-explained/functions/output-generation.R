# Output generation for proportion explained

#' Report Surrogate Statistics
#'
#' This function takes in surrogate data, processes it, and generates a CSV report on the surrogate statistics. 
#' It also outputs a formatted table showing the surrogacy criteria and saves it as a PNG file.
#'
#' @param surrog.in List. A list of surrogate data frames.
#' @param highlight String. Optional. A specific comparator to highlight in the report.
#' @return A gt table object summarizing surrogate statistics.
#' @examples
#' # Example usage
#' report.surrog.statistics(list(df1, df2), highlight = "ComparatorA")
#' @export
report.surrog.statistics <- function(surrog.in, highlight=NULL) {
  
  # If no surrogate data is provided, return NULL
  if (length(surrog.in) == 0) {
    return(NULL)
    # If only one surrogate data frame is provided, use it directly
  } else if (length(surrog.in) == 1) {
    temp <- surrog.in[[1]]
    # If two surrogate data frames are provided, label them and bind them together
  } else if (length(surrog.in) == 2) {
    surrog.in[[1]]$`Image.Quality` <- names(surrog.in)[1]
    surrog.in[[2]]$`Image.Quality` <- names(surrog.in)[2]
    temp <- bind_rows(surrog.in)
  }
  
  # Write a CSV report
  write.csv(temp, path(output_dir, paste0("9c report.surrog.statistics-", reader_type, "-", this_endpoint, ".csv")), row.names=FALSE)
  
  # Get some basic info from the first row of the temp data frame
  vce <- temp$VCE[1]
  data_set <- temp$data_set[1]
  reader_type <- temp$reader_type[1]
  
  # Define hypothesis labels
  for.lables <- c(temp$hypothesis[1], temp$hypothesis[2], temp$hypothesis[3], temp$hypothesis[4], temp$hypothesis[5],
                  temp$Term[6], temp$Term[7], temp$Term[8])
  
  # Modify hypotheses and labels in the data frame
  temp <- temp %>% dplyr::mutate(
    hypothesis=ifelse(is.na(hypothesis)==TRUE, Term, hypothesis),
    comparatorT=label_for_doc.vec(comparatorT),
    hypothesis=factor(hypothesis, for.lables))
  
  levels(temp$hypothesis)[1:5] <- c("Effect of S on T",
                                    "Effect of Z on S",
                                    "Effect of Z on T",
                                    "Effect of S on T after adjusting for Z",
                                    "Effect of Z on T after adjusting for S")
  
  # Write another CSV report
  write.csv(temp, file=path(table_path, paste0("surrog.statistics-", vce, "-", reader_type, ".csv")))
  append.csv(temp %>% dplyr::mutate(this_endpoint=vce, reader_type=reader_type, stratum="<pooled>"), path=path(output_dir, paste("analysis-summary", study, Sys.Date(), ".csv")))
  
  # Filter by the highlight if provided
  if (!is.null(highlight)) {
    temp <- temp %>% dplyr::filter(grepl(sub(" Change$", "", highlight), comparatorT))
    for.title <- paste0("Surrogacy Criteria for ", vce, " as surrogate for ", highlight, " treated as Comparator")
    filename_suffix <- paste0(highlight)
  } else {
    for.title <- paste0("Surrogacy Criteria for ", vce, " as surrogate for various comparators")
    filename_suffix <- "various"
  }
  
  # Prepare the gt table
  table.out <- temp %>%
    dplyr::select(-Std.Error, -model_type, -data_set, -reader_type, -VCE, -Term, -Statistic) %>%
    group_by(comparatorT, Approach) %>%
    dplyr::select(hypothesis, everything()) %>%
    gt() %>%
    cols_merge(columns=c(Estimate, LCL, UCL), pattern="{1} [{2}, {3}]") %>%
    cols_label(Estimate="Estimate [95% CI]") %>%
    cols_label(LCL="Confidence Interval") %>%
    cols_align(align="center", columns=everything()) %>%
    fmt_number(columns=c(Estimate, LCL, UCL, `p-value`), decimals=2) %>%
    tab_footnote(footnote="S: Surrogate, T: Comparator, Z: Intervention") %>%
    tab_header(
      title=for.title,
      subtitle=paste0("Reader Type: ", reader_type)
    ) %>%
    cols_label(hypothesis="Hypothesis/Metric")
  
  # Save the gt table as a PNG file
  gt::gtsave(table.out, filename=path(table_path, paste0("surrog-table of S (", this_endpoint, ") and comparators-", filename_suffix, "-full.png")))
  
  return(table.out)
}
#' Create and Save Surrogacy Criteria Summary Figures
#'
#' Generates summary figures for surrogacy criteria for each unique value of Image.Quality 
#' using ggplot2 and saves each figure as a PNG file.
#'
#' @param surrog.in List. A list containing surrogacy data.
#' @return A list of ggplot objects with summarized surrogacy criteria.
#' @examples
#' # Example usage
#' surrog.criteria.exec.summary.fig(surrog.in)
#' @export
surrog.criteria.exec.summary.fig <- function(surrog.in) {
  
  # Extract reader type and surrogacy variables from the input
  reader_type <- surrog.in$`_heading`$subtitle
  
  # Extract surrogate variable
  S <- gsub(".*Surrogacy Criteria for (.*) as surrogate for.*", "\\1", surrog.in$`_heading`$title)
  
  # Extract data frame from provided list
  temp <- surrog.in[[1]]
  
  # Reverse levels for hypothesis factor
  temp$hypothesis <- factor(temp$hypothesis, rev(levels(temp$hypothesis)))
  
  # Set factor levels for 'Approach' variable
  temp$Approach <- factor(temp$Approach, levels=c("Prentice Criteria", "Freedman's Extension", "Buyse and Molenberghs' Extensions"))
  
  # Define vertical lines for the plot
  vlines.df <- data.frame(Approach=c("Prentice Criteria","Freedman's Extension", "Buyse and Molenberghs' Extensions", "Buyse and Molenberghs' Extensions"),
                          comparatorT="Disease Burden", xintercept=c(0,1,1,-1))
  vlines.df$Approach <- factor(vlines.df$Approach, levels=c("Prentice Criteria", "Freedman's Extension", "Buyse and Molenberghs' Extensions"))
  
  # Define shading data for the plot
  shading_data <- data.frame(Approach=factor(
    c("Prentice Criteria", "Freedman's Extension", "Buyse and Molenberghs' Extensions"),
    c("Prentice Criteria", "Freedman's Extension", "Buyse and Molenberghs' Extensions")),
    xmin=c(0, 0.5, 0.5), xmax=c(Inf, 1.25, 1.5), ymin=c(-Inf, -Inf, -Inf), ymax=c(Inf, Inf, Inf), fill=c("green"))
  
  # Initialize a list to store the plots
  plot_list <- list()
  
  # Loop through each unique value of 'Image.Quality'
  for(image_quality in unique(temp$Image.Quality)) {
    # Filter data for the current image quality
    filtered_temp <- temp[temp$Image.Quality == image_quality, ]
    
    # Extract comparator variable
    T <- filtered_temp$comparatorT[1]
    
    # Generate the ggplot
    plot <- filtered_temp %>% ggplot(aes(x=Estimate, xmin=LCL, xmax=UCL, y=hypothesis)) +
      geom_point() + scale_color_manual(values=cbp2) +
      geom_errorbarh() + xlim(-1, 3) +
      geom_vline(data=vlines.df, aes(xintercept=xintercept), linetype=2) +
      labs(
        title=str_wrap(paste0("Surrogacy Criteria for ", S, " as surrogate for ", T), width=30),
        subtitle=paste0(reader_type, " - Image Quality: ", image_quality),
        y="Hypotheses/Metrics", x="Estimate [95% CI]"
      ) + scale_y_discrete(labels=function(x) str_wrap(x, width=22)) +
      facet_wrap(~Approach, scales="free_y", ncol=1) +
      force_panelsizes(rows=c(5, 1, 2), cols=1) +
      theme(plot.title=element_text(size=10)) +
      geom_rect(data=shading_data, aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf),
                fill="green", alpha=0.2, inherit.aes=FALSE)
    
    # Save the generated plot as a PNG file
    filename <- path(figure_path, paste0("surrog-criteria-summary-fig-exec-", S, " for ", T, "-", reader_type, "-image_quality-", image_quality, "-full.png"))
    ggsave(plot, filename=filename)
    
    # Add the plot to the list
    plot_list[[image_quality]] <- plot
  }
  
  # Return the list of ggplot objects
  return(plot_list)
}
# Update this code to handle case where surrog.in$Image.Quality distinct values.


#' Create and Save Surrogacy Criteria Summary Figures
#'
#' Generates summary figures for surrogacy criteria using ggplot2 and saves each figure as a PNG file.
#' 
#' @param surrog.in List. A list containing surrogacy data.
#' @return A list of ggplot objects with summarized surrogacy criteria.
#' @examples
#' # Example usage
#' surrog.criteria.summary.fig(surrog.in)
#' @export
surrog.criteria.summary.fig <- function(surrog.in) {
  
  # Extract reader type and surrogacy variables from the input
  reader_type <- surrog.in$`_heading`$subtitle
  
  # Extract surrogate variable
  S <- gsub(".*Surrogacy Criteria for (.*) as surrogate for.*", "\\1", surrog.in$`_heading`$title)
  
  # Extract data frame from provided list
  temp <- surrog.in[[1]]
  
  # Reverse levels for hypothesis factor
  temp$hypothesis <- factor(temp$hypothesis, rev(levels(temp$hypothesis)))
  
  # Set factor levels for 'Approach' variable
  temp$Approach <- factor(temp$Approach, levels=c("Prentice Criteria", "Freedman's Extension", "Buyse and Molenberghs' Extensions"))
  
  # Define vertical lines for the plot
  vlines.df <- data.frame(expand.grid(Approach=unique(temp$Approach), comparatorT=unique(temp$comparatorT))) %>%
    arrange(Approach) %>%
    dplyr::mutate(xintercept=ifelse(Approach=="Prentice Criteria", 0, 1))
  
  # Custom labels for plots
  custom_labels.v <- c("Freedman's Extension"="FE", "Buyse and Molenberghs' Extensions"="BME")
  custom_labels.h <- c("Aggregate Histology"="AH", "Disease Burden"="DB", "GI Severity Score"="GISS", "IEL Count"="IEL")
  
  # Initialize a list to store the plots
  plot_list <- list()
  
  # Loop through each unique value of 'Image.Quality'
  for(image_quality in unique(temp$Image.Quality)) {
    # Filter data for the current image quality
    filtered_temp <- temp[temp$Image.Quality == image_quality, ]
    
    # Extract comparator variable
    T <- filtered_temp$comparatorT[1]
    
    shading_data <- data.frame(Approach=factor(
      c("Prentice Criteria", "Freedman's Extension", "Buyse and Molenberghs' Extensions"),
      c("Prentice Criteria", "Freedman's Extension", "Buyse and Molenberghs' Extensions")),
      xmin=c(0, 0.5, 0.5), xmax=c(Inf, 1.25, 1.5), ymin=c(-Inf, -Inf, -Inf), ymax=c(Inf, Inf, Inf), fill=c("green"))
    
    
    # Generate the ggplot
    plot1 <- filtered_temp %>%
      ggplot(aes(x=Estimate, xmin=LCL, xmax=UCL, y=hypothesis)) +
      geom_point() + scale_color_manual(values=cbp2) +
      geom_errorbarh() + xlim(-1, 3) +
      geom_vline(data=vlines.df, aes(xintercept=xintercept), linetype=2) +
      labs(
        title=paste0("Surrogacy Criteria for ", S, " as surrogate for various comparators"),
        subtitle=paste0(reader_type, " - Image Quality: ", image_quality),
        y="Hypotheses/Metrics", x="Estimate [95% CI]"
      ) + scale_y_discrete(labels=function(x) str_wrap(x, width=22))  +
      facet_grid(Approach ~ comparatorT, scales="free_y", labeller=labeller(Approach=custom_labels.v, comparatorT=custom_labels.h)) +
      force_panelsizes(rows=c(5, 1, 2), cols=c(1,1,1)) +
      labs(caption="AH: Aggregate Histology; DB: Disease Burden; GISS: GI Severity Score; IEL: IEL Count\nFE: Freedman's Extension; BME: Buyse and Molenberghs' Extensions") +
      theme(axis.text.y=element_text(size=8), plot.title=element_text(size=10)) +
      geom_rect(data=shading_data, aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf),
                fill="green", alpha=0.2, inherit.aes=FALSE)
    
    # Save the generated plot as a PNG file
    filename <- path(figure_path, paste0("surrog-criteria-summary-fig-", S, "-for-", T, "-", reader_type, "-image_quality-", image_quality, ".png"))
    ggsave(plot1, filename=filename)
    
    # Add the plot to the list
    plot_list[[image_quality]] <- plot1
  }
  
  # Return the list of ggplot objects
  return(plot_list)
}

model.metrics.fig <- function(model.metric.in) {
  if(length(model.metric.in) == 0){
    return(NULL)
  } else if(length(model.metric.in) == 1){
    temp <- model.metric.in[[1]]
  } else if(length(model.metric.in) == 2){
    model.metric.in[[1]]$`Image.Quality` <- names(model.metric.in)[1]
    model.metric.in[[2]]$`Image.Quality` <- names(model.metric.in)[2]
    temp <- bind_rows(model.metric.in)
  }

  temp <- temp %>% dplyr::mutate(model.components=sub(".*Comparator:", "", model.components))
  temp$model.components <- label_for_doc.vec(temp$model.components)
  
  temp %>% ggplot(aes(x=Endpoint, y=Value, shape=Metric)) + geom_point() + 
    facet_wrap(~model.components, scales="free_y") + theme(aspect.ratio=1) +
    scale_color_manual(values=cbp2) +
    scale_y_continuous(limits=c(0, NA)) + 
    geom_hline(yintercept=.5, linetype=2) + coord_flip() +
    labs(title="Model Fit Metrics", subtitle=paste0("Reader Type: ", temp$reader_type[1]), x=NULL, y=NULL) +
    theme(legend.position="bottom")
}

prentice.fig.suite <- function(data.in, this_endpoint, comparatorT, model.in, dataset.descript) {
  reader_type <- model.in$altogether$reader_type[1]
  # Figure for regress of S on T
  slope <- model.in$model.report %>% dplyr::filter(model_type=="Linear Model without treatment") %>% slice(2) %>% dplyr::pull(Estimate)
  slope.CI <- model.in$model.report %>% dplyr::filter(model_type=="Linear Model without treatment") %>% slice(2) %>% dplyr::select(LCL, UCL) %>% unlist()
  intercept <- model.in$model.report %>% dplyr::filter(model_type=="Linear Model without treatment") %>% slice(1) %>% dplyr::pull(Estimate)
  S.on.T <- data.in %>% ggplot(aes(x=!!sym(this_endpoint), y=!!sym(comparatorT))) +
    geom_point() + theme(aspect.ratio=1) + scale_color_manual(values=cbp2) +
    geom_abline(intercept=intercept, slope=slope) +
    labs(title=paste0("Change in ", label_for_doc(this_endpoint), " vs. Change in ", label_for_doc(comparatorT)), 
         subtitle=paste0("Without regard for treatment\n", dataset.descript, "\n", "Reader Type: ", reader_type),
         caption=paste0("Slope from Simple Linear Model: ", round(slope, 2), "; Intercept: ", round(intercept, 2), "\n", "95% CI for slope: [", round(slope.CI[1], 2), ", ", round(slope.CI[2], 2), "]"),
         x=paste0("Change in ", label_for_doc(this_endpoint)), y=paste0("Change in ", label_for_doc(comparatorT)))
  ggsave(filename=path(figure_path, paste0("prentice-S (", this_endpoint, ") on T (", comparatorT, ")-full-", model.in$altogether$data_set[1], ".png")))
 
  for.caption <-  model.in$model.report %>% dplyr::filter(model_type=="Bivariate Regression Model") %>% slice(2,4) %>%
    dplyr::mutate(label=paste0("Treatment effect [95% CI] on ", sub("_.*", "", Term), " endpoint: ", round(Estimate,2), " [", round(LCL,2), ", ", round(UCL,2), "]")) %>% dplyr::select(label)
  for.caption[1,1] <- gsub(x=for.caption[1,1], pattern=" endpoint", replacement="")
 
  Z.on.S.and.on.T.title <- str_wrap(paste0(label_for_doc(this_endpoint), " and ", label_for_doc(comparatorT), " Change"),width=30)
  Z.on.S.and.on.T <- data.in %>% pivot_longer(cols=2:3) %>% ggplot(aes(x=ARM, y= value, color=ARM)) +
    geom_boxplot() + geom_point() + facet_wrap(~name, scales="free_y") +  scale_color_manual(values=cbp2) + 
    labs(title=Z.on.S.and.on.T.title, 
          subtitle=paste0(dataset.descript, "\n", reader_type),
          caption=paste0("Treatment effects from Bivariate Regression Model\n", for.caption$label[1], "\n", for.caption$label[2]),
         x="Treatment Group", y="Change in Endpoint")+scale_color_manual(values=cbp2) +
    theme(legend.position="bottom")
  ggsave(filename=path(figure_path, paste0("prentice-Z on S (", this_endpoint, ") and T (", comparatorT, ")-full-",  model.in$altogether$data_set[1], ".png")))
 
  current.model <- model.in$model.report %>% dplyr::filter(model_type == "Linear Model with treatment")
  reference.arm <- setdiff(unique(data.in$ARM),  current.model %>% slice(2) %>% dplyr::pull(Term) %>% gsub("ARM", "", .))
  other.arm <- current.model %>% slice(2) %>% dplyr::pull(Term) %>% gsub("ARM", "", .)
  reg.lines <- data.frame(ARM=c(reference.arm, other.arm), slope=current.model %>% slice(3) %>% dplyr::pull(Estimate)) %>% 
    dplyr::mutate(intercept=c(current.model$Estimate[1], current.model$Estimate[1] + current.model$Estimate[2])) 
  slope.report <- current.model %>% slice(3) %>% dplyr::select(Estimate, LCL, UCL) %>% unlist()
  treatment.report <- current.model %>% slice(2) %>% dplyr::select(Estimate, LCL, UCL) %>% unlist()
 
  S.on.T.adj.Z.title <- str_wrap(paste0("Change in ", label_for_doc(this_endpoint), " vs. Change in ", label_for_doc(comparatorT)),width=30)
  
  S.on.T.adj.Z <-  data.in %>% ggplot(aes(x=!!sym(this_endpoint), y=!!sym(comparatorT), color=ARM)) +
    geom_point() + theme(aspect.ratio=1) + scale_color_manual(values=cbp2) +
    geom_abline(data=reg.lines, aes(intercept=intercept, slope=slope, color=ARM), linetype=2, linewidth=1) +
    labs(title=S.on.T.adj.Z.title,
         subtitle=paste0(dataset.descript, "\nReader Type:", reader_type),
         x=paste0("Change in ", label_for_doc(this_endpoint)), y=paste0("Change in ", label_for_doc(comparatorT)),
         caption=paste0("Components from Extended Linear Model\n", "Treatment effect [95% CI] on ", comparatorT, ": ", round(treatment.report[1], 2), " [", round(treatment.report[2], 2), ", ", round(treatment.report[3], 2), "]\n",
                        "Slope for change in Suggogate: ", round(slope.report[1], 2), " [", round(slope.report[2], 2), ", ", round(slope.report[3], 2), "]")) +  
    theme(legend.position="bottom")
  
  Z.on.S.and.on.T <- Z.on.S.and.on.T+ theme(axis.title=element_text(size=11), 
        axis.text=element_text(size=10),
        legend.text=element_text(size=10),
        plot.title=element_text(size=10,face="bold"),
        plot.subtitle=element_text(size=10),
        plot.caption=element_text(size=7)) +
    scale_x_discrete(breaks=NULL) + labs(x=NULL)
  
  S.on.T.adj.Z <- S.on.T.adj.Z+ theme(axis.title=element_text(size=11), 
        axis.text=element_text(size=10),
        legend.text=element_text(size=10),
        plot.title=element_text(size=10,face="bold"),
        plot.subtitle=element_text(size=10),
        plot.caption=element_text(size=7)) 
  # holdit <- arrangeGrob(Z.on.S.and.on.T, S.on.T.adj.Z, ncol=2)
  combined_plot <- Z.on.S.and.on.T | S.on.T.adj.Z 
  ggsave(combined_plot, filename=path(figure_path, paste0("prentice-S (", this_endpoint, ") on T (", comparatorT, ") adjZ-full-", model.in$altogether$data_set[1], ".png")), height=8.5, width=11, units="in", dpi=300)
  
  return(list(combo=combined_plot, S.on.T=S.on.T, Z.on.S.and.on.T=Z.on.S.and.on.T, S.on.T.adj.Z=S.on.T.adj.Z))
}

generate_interval.distributions_plot <- function(global_analysis_set) {
  arms <- length(unique(na.omit(global_analysis_set$ARM)))
  for.figure <- intersect(c(this_endpoint, unlist(comparators)), colnames(global_analysis_set))
    temp.df <- global_analysis_set %>%
      dplyr::select(unlist(for.figure), pairing, ARM)
    write.csv(temp.df, path(output_dir, paste0("7b just before pivot-", "ML", ".csv")), row.names=FALSE)
    temp.df <- temp.df %>% 
      ungroup() %>%
      dplyr::mutate(VHCD=-VHCD) %>% 
      dplyr::mutate(across(where(is.numeric), ~ (. - mean(., na.rm=TRUE)) / sd(., na.rm=TRUE))) %>%
      pivot_longer(cols=-intersect(colnames(temp.df), c("ARM","pairing","STUDYID","SITE","SUBJID")), 
                   names_to="Endpoint", values_to="Value")
    write.csv(temp.df, path(output_dir, paste0("7c pivot_longer output-", "ML", ".csv")), row.names=FALSE)
    temp.df$Endpoint <- factor(temp.df$Endpoint, levels=unique(temp.df$Endpoint), labels=label_for_doc.vec(unique(temp.df$Endpoint)))
    for.subtitle <- ifelse(!is.null(global_analysis_set$label.name[1]), paste0("Reader type: ML\n", global_analysis_set$label.name[1]),"Reader type: ML" )
    levels(temp.df$Endpoint) <- label_for_doc.vec(c("BVA", "Vh:Cd", "IELCOUNT", "GISS", "VCIEL", "Agg_Histology", "Disease_Burden"))
    variable.distributions <- temp.df %>% dplyr::filter(Endpoint !="VCIEL") %>% dplyr::filter(Endpoint !="Agg_Histology") %>%
      ggplot(aes(x=pairing, y=Value, color=ARM)) +
      geom_boxplot() + guides(color="none") +
      theme(legend.position="bottom") + scale_color_manual(values=cbp2) + 
      labs(x=NULL, y="Scaled Value", title=paste0("Distributions of Variable Change, by Interval"),
           subtitle=for.subtitle)+
      facet_wrap(Endpoint ~ ARM, labeller=label_wrap_gen(multi_line=FALSE, width=40), ncol=arms) + theme(axis.text.x=element_text(angle=20, hjust=1, vjust=1))
    ggsave(filename=path(figure_path, paste0("variable.distributions.png")), height=9, width=6, units="in", dpi=300)
  return(variable.distributions)
}
