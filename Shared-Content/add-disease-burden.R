# Various analyses incorporate the presence of multiple clinical markers rather than only one; we 
# create a latent true disease burden measure to reflect the severity of the disease, combining 
# multiple clinical and biological outcomes on the same patient at the same time. To make this 
# concrete, we use at least Vh:Cd, IEL count, and GISS as separate observations, each suggesting 
# a severity of disease, but individually, each is imperfect. Together they are understood to provide
# a more reliable determination than any of them individually. Analyses that use this latent true
# disease burden clinical measure include:
#
# • It is the basis of a more robust determination of disease severity when selecting data for the
#   bias and linearity assessment within the Analytical Validation.
# • Along with the various single-variable associations within the Clinical/Biological Validation,
#   association with this latent true disease burden is also used (in fact, potentially being the
#   more informative association of all, as it avoids over-reliance on any of the imperfect single
#   variables). This is done at the individual time points as well as the assessment of change
#   across timepoints.
# • It also serves as the true endpoint in the surrogacy assessment of proportion explained for the
#   Response CoU. 
#

add.latent_disease_burden_comparators <- function(df.to_be_extended,output_dir) {
  temp <- df.to_be_extended %>% 
      dplyr::ungroup() %>%
      dplyr::summarize(VHCD.mean=mean(VHCD, na.rm=TRUE), IELCOUNT.mean=mean(IELCOUNT, na.rm=TRUE),
                       VHCD.sd=sd(VHCD, na.rm=TRUE), IELCOUNT.sd=sd(IELCOUNT, na.rm=TRUE))
    
  vhcd_z <- (df.to_be_extended$VHCD - temp$VHCD.mean) / temp$VHCD.sd  
  ielcount_z <- (df.to_be_extended$IELCOUNT - temp$IELCOUNT.mean) / temp$IELCOUNT.sd
  df.to_be_extended$Agg_Histology <- -(sqrt(2)/2 * vhcd_z) + (sqrt(2)/2 * ielcount_z)
  df.to_be_extended$VCIEL <- df.to_be_extended$VHCD/sd(df.to_be_extended$VHCD,na.rm=TRUE) - df.to_be_extended$IELCOUNT/sd(df.to_be_extended$IELCOUNT,na.rm=TRUE)

    
  if("GISS" %in% colnames(df.to_be_extended)) {
    temp <- df.to_be_extended %>% 
      dplyr::summarize(GISS.mean=mean(GISS, na.rm=TRUE), GISS.sd=sd(GISS, na.rm=TRUE))
  
    giss_z <- (df.to_be_extended$GISS - temp$GISS.mean) / temp$GISS.sd
    df.to_be_extended$Disease_Burden <- df.to_be_extended$Agg_Histology + (1.0 * giss_z)
  } else {
    df.to_be_extended$Disease_Burden <- NA
  }
 
  return(df.to_be_extended)
}
