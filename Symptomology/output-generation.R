stability.stats.report <- function(data.in = for.return$stability_stats){

    # Create the gt table
    gt_table <- data.in %>%
      gt() %>%
      tab_header(
        title = "Stability Statistics",
        subtitle = "Questionnaire Results"
      ) %>%
      fmt_number(
        columns = c(mean, sd, cv, snr),
        decimals = 2
      ) %>%
      cols_label(
        mean = "Mean",
        sd = "Standard Deviation",
        cv = "Coefficient of Variation",
        snr = "Signal-to-Noise Ratio"
      ) %>%
      tab_style(
        style = cell_text(align = "center"),
        locations = cells_body(columns = everything())
      ) 
    
    gt::gtsave(
      data = gt_table,
      filename = path(table_path, paste0("stability-stats-report.png"))
    )
    
    return(gt_table)
}

cluster.plots <- function(data.in = for.return$CDSD.cluster){
  grid.arrange(data.in$plot + 
                 theme(legend.position = "bottom") +
                 guides(color = guide_legend(nrow = 1)), data.in$path.plot + theme(legend.position = "bottom"), ncol = 2, widths=c(.7,.3))
}



