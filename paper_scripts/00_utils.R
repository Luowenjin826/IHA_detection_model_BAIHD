#This file contains R utility functions used throughout the project.

#missing rate
compute_missing_rate <- function(df) {
  missing_rates <- colSums(is.na(df)) / nrow(df)
  missing_rates <- round(missing_rates,2)
  dat <- data.frame(MissingRate = missing_rates)
  return(dat)
}

#density plot for raw and imputed data 
plot_density <- function(imp, df) {
  long <- complete(imp, "long", include = TRUE)
  
  long$.imp <- factor(long$.imp, levels = c(0, 1:imp$m),
                      labels = c("Raw", rep("Imputed", imp$m)))
  
  for (var in names(df)) {
    p <- ggplot(long, aes(x = get(var), color = .imp)) +
      geom_density(linewidth = 0.8) +  
      labs(title = "",
           x = var, y = "Density", color = "Data") +
      theme_test() +
      theme(legend.position = c(0.95, 0.95),  
            legend.justification = c(1, 1),
            legend.title = element_blank(), 
            legend.text = element_text(size = 12),
            axis.title.x = element_text(face = "bold",size = 16),
            axis.title.y = element_text(face = "bold",size = 16),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14)) 
    print(p)
  }
}

#data normalization
z_score<-function(x){
  z<-(x - mean(x,na.rm = T))/sd(x,na.rm = T)
  return(z)
}

trans1<-function(x){
  dt1<-data.frame(x)
  dt1$TG<-log(dt1$TG)
  dt1$FBG<-log(dt1$FBG)
  dt1$PRC<-log(dt1$PRC)
  dt1[,2:10]<-apply(dt1[,2:10],2,z_score) %>% round(.,3)
  return(dt1)
}
trans2<-function(x){
  dt1<-data.frame(x)
  dt1$TG<-log(dt1$TG)
  dt1$FBG<-log(dt1$FBG)
  dt1$PRC<-log(dt1$PRC)
  dt1[,1:9]<-apply(dt1[,1:9],2,z_score) %>% round(.,3)
  return(dt1)
}

#cox regression in ukb cohort
perform_cox_regression <- function(formula, data) {
  # Validate input data
  if (!all(c("surv_time", "outcome") %in% colnames(data))) {
    stop("Input data must contain 'surv_time' and 'outcome' columns.")
  }
  
  # Fit Cox model
  fit <- coxph(formula, data = data)
  summary_fit <- summary(fit)
  
  # Extract results
  results <- data.frame(
    case_total = paste0(sum(data$outcome == 1), "/", nrow(data)),
    hazard_ratio = sprintf("%0.2f", summary_fit$coefficients[1, 2]),
    lower_ci = sprintf("%0.2f", summary_fit$conf.int[1, 3]),
    upper_ci = sprintf("%0.2f", summary_fit$conf.int[1, 4]),
    p_value = ifelse(summary_fit$coefficients[1, 5] < 0.0001, 
                     " < 0.0001", 
                     sprintf("%0.4f", summary_fit$coefficients[1, 5]))
  )
  
  # Format results
  results <- results %>%
    mutate(HR_95_CI = paste0(hazard_ratio, " (", lower_ci, ", ", upper_ci, ")")) %>%
    select(case_total, HR_95_CI, p_value, hazard_ratio, lower_ci, upper_ci)
  
  return(results)
}

#logistic regression in fos cohort
perform_logistic_regression <- function(formula, data) {
  # Validate input data
  if (!("outcome" %in% colnames(data))) {
    stop("Input data must contain an 'outcome' column.")
  }
  
  # Fit logistic model
  fit <- glm(formula, family = 'binomial', data = data)
  summary_fit <- summary(fit)
  
  # Extract coefficients
  beta <- summary_fit$coefficients[2, 1]
  sd <- summary_fit$coefficients[2, 2]
  beta_lower <- beta - 1.96 * sd
  beta_upper <- beta + 1.96 * sd
  p_value <- summary_fit$coefficients[2, 4]
  
  # Format results
  results <- data.frame(
    odds_ratio = exp(beta),
    lower_ci = exp(beta_lower),
    upper_ci = exp(beta_upper)
  ) %>%
    round(2) %>%
    mutate(
      case_total = paste0(sum(data$outcome == 1), "/", nrow(data)),
      OR_95_CI = paste0(odds_ratio, " (", lower_ci, ", ", upper_ci, ")"),
      p_value = ifelse(p_value < 0.0001, " < 0.0001", sprintf("%0.4f", p_value))
    ) %>%
    select(case_total, OR_95_CI, p_value, odds_ratio, lower_ci, upper_ci)
  
  return(results)
}

#table for forestplot
create_forestplot_table <- function(results, labels) {
  table <- matrix(NA, ncol = 7, nrow = 7) %>% as.data.frame()
  table[c(3, 4, 6, 7), 2:7] <- results
  table[, 1] <- labels
  table[1, 2:4] <- c("No. of case/total", "HR/OR (95% CI)", "P value")
  return(table)
}

# Generate forest plots
generate_forestplot <- function(table, title, xlab, output_file) {
  forestplot(
    as.matrix(table[, 1:4]),
    mean = as.numeric(table[, 5]),
    lower = as.numeric(table[, 6]),
    upper = as.numeric(table[, 7]),
    colgap = unit(5, 'mm'),
    lineheight = unit(15, 'mm'),
    graph.pos = 2,
    lwd.zero = 1,
    lwd.ci = 2,
    ci.vertices = FALSE,
    col = fpColors(box = "#ff6347", lines = "#4682b4", zero = '#444444', axes = "#444444"),
    zero = 1,
    boxsize = 0.3,
    xlab = xlab,
    lwd.xaxis = 1,
    xticks = c(0, 1, 2, 3, 4),
    is.summary = c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE),
    hrzl_lines = list('2' = gpar(lty = 1)),
    graphwidth = unit(0.25, "npc"),
    txt_gp = fpTxtGp(label = gpar(cex = 1), ticks = gpar(cex = 1), xlab = gpar(cex = 1)),
    line.margin = 1
  )
}

#generate table one
table1<-function(df){
  dt1<-data.frame(df)
  for (i in 11:12){
    dt1[,i]<-factor(dt1[,i])
  }
  
  vars<-colnames(dt1)[c(2,11,3:9,13,10,14,12)]
  nonnormal_vars<-c('TG','FBG','PAC','PRC','ARR')
  dt2<-CreateTableOne(vars = vars,
                      data = dt1) 
  dt2<-data.frame(print(dt2,
                        nonnormal = nonnormal_vars))
  return(dt2)
}

#compute least squares means and format results
table_lsm <- function(df) {
  # Create a copy of the data frame and apply log transformation
  dt1 <- df %>%
    mutate(
      ARR_log = log2(ARR + 1e-8),
      PAC_log = log2(PAC + 1e-8),
      PRC_log = log2(PRC + 1e-8)
    )
  
  # Fit linear models
  model1 <- lm(PAC_log ~ IHA + Age + Sex, data = dt1)
  model2 <- lm(PRC_log ~ IHA + Age + Sex, data = dt1)
  model3 <- lm(ARR_log ~ IHA + Age + Sex, data = dt1)
  
  # Function to calculate least squares means and format results
  calculating_lsm <- function(model) {
    lsm <- lsmeans(model, "IHA")
    comparison <- data.frame(pairs(lsm))
    
    # Format results
    lsm_results <- lsm %>%
      data.frame() %>%
      select(lsmean, lower.CL, upper.CL) %>%
      mutate(
        lsm_value = paste0(
          round(2^lsmean, 1), 
          " (", round(2^lower.CL, 1), ",", round(2^upper.CL, 1), ")"
        )
      )
    
    # Create a data frame with formatted results
    data.frame(
      PH = lsm_results[1, "lsm_value"],
      IHA = lsm_results[2, "lsm_value"],
      pvalue = comparison[1, "p.value"]
    )
  }
  
  # Combine results for all models
  rbind(
    calculating_lsm(model1),
    calculating_lsm(model2),
    calculating_lsm(model3)
  )
}

#compare blood pressure of primary hypertension and predicted IHA in EIMDS, FOS and UKB
compare_bp<-function(x){
  dt1<-data.frame(x)
  vars<-c('SBP','DBP')
  dt2<-CreateTableOne(vars = vars,
                      strata = "IHA",
                      data = dt1) 
  dt2<-data.frame(print(dt2))
  dt2<-dt2[-1,]
  return(dt2)
}

#create forest plots of sensitivity and specificity
create_forest_plot <- function(data, title, color, xlim, annotations) {
  ggplot(data, aes(mean, Cohort)) +
    geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.1, color = color) +
    geom_pointrange(aes(xmin = lower, xmax = upper), size = 1.3, color = color) +
    xlim(xlim[1], xlim[2]) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(color = 'black', fill = 'transparent'),
      axis.text.x = element_text(color = "black", size = 13),
      axis.text.y = element_text(color = "black", size = 13),
      axis.ticks.length = unit(0.2, "cm"),
      axis.ticks = element_line(color = "black", size = 1),
      plot.title = element_text(hjust = 0.5, size = 16)
    ) +
    labs(title = title, x = "", y = "") +
    annotate("text", x = annotations$x, y = annotations$y, label = annotations$labels, 
             size = 4.8, color = "black")
}
