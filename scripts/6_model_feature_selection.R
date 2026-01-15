################################################################################
#Modelling data (OOB validation)
################################################################################


#Removing primary independent variables
#Removing derived, scaling, and correlative variables
data_rf <- data_eda%>%
  filter(!year == 2015)%>%
  select(-c(year, local_authority, average_download_speed_mbit_s, 
            average_upload_speed_mbit_s, births, deaths, total_emp, growth_group))
  

################################################################################
#Building bootstrapped random forest model
################################################################################


set.seed(123456) #For reproducibility


B <- 500 #Number of bootstraps


#Setting variable names
var_names <-setdiff(names(data_rf), "emp_growth_pct")
#Creating feature importance matrix
feat_import_matrix <- matrix(NA, nrow = B, ncol=length(var_names), dimnames = list(NULL, var_names))

for(b in 1:B){
  
  #Bootstrap sample
  idx <- sample(seq_len(nrow(data_rf)), replace = TRUE)
  boot_rf <- data_rf[idx, ]
  
  rf <- randomForest(emp_growth_pct ~ .,
                     data = boot_rf,
                     importance = TRUE)
  
  feat_import_matrix[b, ] <- importance(rf)[, "%IncMSE"]

  
}


################################################################################
#Summarising initial feature importance test
################################################################################

feat_importance_summary <- data.frame(
  variable = var_names,
  mean_importance = colMeans(feat_import_matrix, na.rm= TRUE),
  pct_positive = colMeans(feat_import_matrix > 0, na.rm = TRUE))%>% #Values that had positive importance only
  arrange(desc(mean_importance))

print(rf)
summary(feat_importance_summary)

#Figure 13: Plotting mean_importance above overall_mean threshold
feat_importance_summary <- feat_importance_summary%>%
  mutate(overall_mean = mean(mean_importance))

feat_importance_plot <- ggplot(feat_importance_summary, aes(x = reorder(variable, mean_importance), y = mean_importance))+
  geom_point()+
  geom_hline(aes(yintercept = overall_mean, linetype = "Mean Importance Threshold"))+
  labs(x = "Indicator",
       y = "Importance Ranking",
       linetype = "")+
  scale_linetype_manual(values = c("Mean Importance Threshold" = "dashed"))+
  theme_minimal()+
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, size = 8), 
        legend.position = "top")

print(feat_importance_plot)
ggsave("figure13_rq2.png", plot = feat_importance_plot, path = "outputs/figures")

################################################################################
#Creating ML model
################################################################################


rq_model <- data_eda%>%
  filter(!year == 2015)%>%
  select(year, emp_growth_pct, average_download_speed_mbit_s, average_upload_speed_mbit_s,
         net_business, transport_storage_postal, level_2_attainment_pct, level_3_attainment_pct, 
         hhi, manufacturing, health, finance_insurance)


################################################################################
#Train/test splitting (randomised)
################################################################################


rq_split_index <- createDataPartition(rq_model$emp_growth_pct, p = 0.7,
                                      list = FALSE)

rq_train <- rq_model[rq_split_index, ]
rq_test <- rq_model[-rq_split_index, ]
