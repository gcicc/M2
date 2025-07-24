simulate_data <- function(n_subjects = 31, n_traversals = 2, n_retests = 2,
                          sd_subject = .2, sd_retest = .4, sd_interaction = .68, sd_error = .16) {
  
  # Create empty data frame to store results
  data <- data.frame(
    Subject = rep(1:n_subjects, each = n_traversals * n_retests),
    Traversal = rep(rep(1:n_traversals, each = n_retests), times = n_subjects),
    Retest = rep(1:n_retests, times = n_subjects * n_traversals)
  )
  
  # Simulate subject random effects
  data$SubjectEffect <- rnorm(n_subjects, mean = 0, sd = sd_subject)[data$Subject]
  
  # Simulate retest random effects
  data$RetestEffect <- rnorm(n_retests, mean = 0, sd = sd_retest)[data$Retest]
  
  # Simulate interaction effects
  interaction_matrix <- matrix(rnorm(n_subjects * n_retests, mean = 0, sd = sd_interaction), 
                               nrow = n_subjects, ncol = n_retests)
  data$InteractionEffect <- interaction_matrix[cbind(data$Subject, data$Retest)]
  
  # Simulate intra-reader random error
  data$Error <- rnorm(nrow(data), mean = 0, sd = sd_error)
  
  # Calculate the response variable
  data$Response <- data$SubjectEffect + data$RetestEffect + data$InteractionEffect + data$Error
  
  return(data)
}

# Example usage
set.seed(123)
simulated_data <- simulate_data()
anova.fit <- anovaVCA(Response~(Subject  + Retest  + Subject *Retest), simulated_data)$aov.tab
REML.fit <- fitVCA(Response~(Subject  + Retest  + Subject *Retest), simulated_data, method = "REML")
VCA.total.analytic.variation(VCA.fit = my.VCA.fit, VCA.data.in = simulated_data, alpha = .05)

