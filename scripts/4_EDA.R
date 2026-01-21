################################################################################
#Employment growth (outcome) EDA
################################################################################


#Table3: Employment growth summarised by local authority
table3_eda <- growth_groups%>%
  select(growth_group, local_authority, mean_growth, median_growth, sd_growth)

print(table3_eda)
write_csv(table3_eda, file = "outputs/tables/table3_eda.csv")

#Average growth per group per year
growth_group_year <- data_eda%>%
  group_by(growth_group, year)%>%
  summarise(mean_growth_group = mean(emp_growth_pct, na.rm = TRUE),
            sd_growth_group = sd(emp_growth_pct, na.rm = TRUE),
            n_obs = n(),
            .groups = "drop")%>%
  mutate(se = sd_growth_group / sqrt(n_obs))

#Figure 2: Yorkshire and The Humber LA's Average Employment Growth Trajectories by Employment Growth Group
eda_growth_plot <- growth_group_year%>% 
  filter(!year %in% 2015)%>%
  ggplot(aes(x = year, y = mean_growth_group, colour = growth_group, fill = growth_group))+
  geom_line(linewidth = 1.1)+
  geom_ribbon(aes(ymin = mean_growth_group - se,
                  ymax = mean_growth_group + se),
              alpha = 0.2, colour = NA)+
  scale_colour_manual(values = growth_group_cols)+
  labs(subtitle = "Shaded Bands = ±1 Standard Error",
       x = "Year", y = "Employment Growth (%)", fill = NULL, colour = "Employment Growth Group",
       caption = "Business Register and Employment Survey (2015-2023).")+
  guides(fill = "none")+
  theme_minimal()+
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "bottom",
        legend.direction = "horizontal")

print(eda_growth_plot)
ggsave("figure2_eda.png", plot = eda_growth_plot, path = "outputs/figures")


################################################################################
#Digital connectivity EDA
################################################################################


#LA Digital indicator summary statistics
la_dig_summary <- data_eda%>%
  group_by(local_authority)%>%
  summarise(mean_download = mean(average_download_speed_mbit_s),
            median_download = median(average_download_speed_mbit_s),
            mean_upload = mean(average_upload_speed_mbit_s),
            median_upload = median(average_upload_speed_mbit_s),
            sd_download = sd(average_download_speed_mbit_s),
            sd_upload = sd(average_upload_speed_mbit_s))

print(la_dig_summary)

#Yearly Digital indicator summary statistics
year_dig_summary <- data_eda%>%
  group_by(year)%>%
  summarise(mean_download = mean(average_download_speed_mbit_s),
            median_download = median(average_download_speed_mbit_s),
            mean_upload = mean(average_upload_speed_mbit_s),
            median_upload = median(average_upload_speed_mbit_s),
            sd_download = sd(average_download_speed_mbit_s),
            sd_upload = sd(average_upload_speed_mbit_s))

#Figure 3: Regional Mean Download and Upload Speeds Over Time
eda_dig_means_plot <- year_dig_summary%>%
  pivot_longer(cols = c(mean_download, mean_upload),
               names_to = "metric",
               values_to = "speed")%>%
  ggplot(aes(x = year, y = speed, colour = metric))+
  geom_line(linewidth = 1.5)+
  geom_point()+
  scale_colour_manual(values = c("#F8766D", "#00BFC4"),
                      labels = c("mean_download" = "Download Speed",
                                 "mean_upload" = "Upload Speed"))+
  labs(x = "Year", y = "Speed (Mbit/s)", colour = "",
       caption = "Ofcom Connected Nations (2015-2023)")+
  theme_minimal()+
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "bottom",
        legend.direction = "horizontal")

print(eda_dig_means_plot)
ggsave("figure3_eda.png", plot = eda_dig_means_plot, path = "outputs/figures")


#Figure 4: Distribution of Download Speeds across LAs by Year
eda_dig_dispersion_plot <- data_eda%>%
  ggplot(aes(x = factor(year), y = average_download_speed_mbit_s))+
  geom_boxplot(fill = "#00BFC4")+
  labs(x = "Year",
       y = "Mean Download Speed (Mbit/s)",
       caption = "Ofcom Connected Nations (2015-2023)")+
  theme_minimal()+
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

print(eda_dig_dispersion_plot)
ggsave("figure4_eda.png", plot = eda_dig_dispersion_plot, path = "outputs/figures")


################################################################################
#Industry composition EDA
################################################################################


#Mean industry share per LA across years
mean_industry_share_la <- industry_shares%>%
  group_by(local_authority, industry)%>%
  summarise(mean_share_la = mean(ind_share))%>%
  left_join(select(growth_groups, growth_group, local_authority), by = "local_authority")

#Computing average industry share by growth group
mean_industry_share_group <- mean_industry_share_la%>%
  group_by(growth_group, industry)%>%
  summarise(mean_share_group = mean(mean_share_la))

#Figure 5: Mean Industry Share (High Vs Low Growth LAs)
eda_group_industry_share_plot <- mean_industry_share_group%>%
  filter(growth_group %in% c("High Growth", "Low Growth"))%>%
  ggplot(aes(x = reorder(industry, mean_share_group), y = mean_share_group, fill = growth_group))+
  geom_col(position = "dodge", width = 0.7)+
  scale_fill_manual(values = growth_group_cols)+
  coord_flip()+
  labs(x = "Industry",
       y = "Mean Industry Share (Proportion)",
       fill = "Growth group",
       caption = "Business Register and Employment Survey (2015-2023)")+
  theme_minimal()+
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "top",
        legend.direction = "horizontal")

print(eda_group_industry_share_plot)
ggsave("figure5_eda.png", plot = eda_group_industry_share_plot, path = "outputs/figures")

#HHI summary statistics by local authority
la_hhi_summary <- data_eda%>%
  group_by(local_authority)%>%
  summarise(mean_hhi = mean(hhi),
            median_hhi = median(hhi),
            sd_hhi = sd(hhi))

print(la_hhi_summary)


################################################################################
#Educational data EDA
################################################################################


#LA-level (mean) trends of level 2 & level 3 attainment level percentages over time
la_attainment_summary <- data_eda%>%
  group_by(local_authority)%>%
  summarise(mean_level_2 = mean(level_2_attainment_pct),
            mean_level_3 = mean(level_3_attainment_pct),
            median_level_2 = median(level_2_attainment_pct),
            median_level_3 = median(level_3_attainment_pct),
            sd_level_2 = sd(level_2_attainment_pct),
            sd_level_3 = sd(level_3_attainment_pct))

print(la_attainment_summary)

#Yearly Regional-level (mean) trends of level 2 & level 3 attainment level percentages over time
year_attainment_summary <- data_eda%>%
  group_by(year)%>%
  summarise(mean_level_2 = mean(level_2_attainment_pct),
            mean_level_3 = mean(level_3_attainment_pct),
            median_level_2 = median(level_2_attainment_pct),
            median_level_3 = median(level_3_attainment_pct),
            sd_level_2 = sd(level_2_attainment_pct),
            sd_level_3 = sd(level_3_attainment_pct))


#Figure 6: Mean Level 2 & Level 3 Attainment Over Time (Regional-Level)
eda_attainment_summary_plot <- year_attainment_summary%>%
  pivot_longer(cols = c(mean_level_2, mean_level_3), # need to rename these
               names_to = "level",
               values_to = "attainment_pct")%>%
  ggplot(aes(x = year, y = attainment_pct, colour = level))+
  geom_line(linewidth = 1.2)+
  geom_point()+
  scale_colour_manual(values = c("#F8766D", "#00BFC4"),
                      labels = c("mean_level_2" = "Level 2 Attainment",
                                 "mean_level_3" = "Level 3 Attainment"))+
  labs(x = "Year", y = "Attainment (%)",
       colour = "",
       caption = "Department for Education (2015-2023)")+ # need to make sure correct source
  theme_minimal()+
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "bottom",
        legend.direction = "horizontal")

print(eda_attainment_summary_plot)
ggsave("figure6_eda.png", plot = eda_attainment_summary_plot, path = "outputs/figures")

################################################################################
#Business demography EDA
################################################################################

#Business demography summary statistics by local authority
la_demography_summary <- data_eda%>%
  group_by(local_authority)%>%
  summarise(sum_births = sum(births),
            sum_deaths = sum(deaths),
            sum_net_business = sum(net_business))

#Yearly regional net business change over time
year_demography_summary <- data_eda%>%
  group_by(year)%>%
  summarise(sum_births = sum(births),
            sum_deaths = sum(deaths),
            sum_net_business = sum(net_business))

#Assigning colours to demography groups
demography_group_cols <- c("sum_births" = "#59A14F",
                           "sum_deaths" = "#E15759",
                           "sum_net_business" = "#4E78A7")

#Figure 7: Regional Business Births, Deaths, and Net Change Over Time
eda_demography_summary_plot <- year_demography_summary%>%
  ggplot(aes(x = year))+
  geom_line(aes(y = sum_births, colour = "Births"), linewidth = 1.2)+ #need to make lines thicker without linewidth in legend---------change colour too
  geom_line(aes(y = sum_deaths, colour = "Deaths"), linewidth = 1.2)+
  geom_line(aes(y = sum_net_business, colour = "Net Business"), linewidth = 1.2)+
  scale_colour_manual(values = c("#59A14F", "#E15759", "#4E78A7"))+
  labs(x = "Year",
       y = "Count",
       colour = "",
       caption = "ONS Business Demography (2015-2023)")+
  theme_minimal()+
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "bottom",
        legend.direction = "horizontal")

print(eda_demography_summary_plot)
ggsave("figure7_eda.png", plot = eda_demography_summary_plot, path = "outputs/figures")


################################################################################
#Printing LA summaries
################################################################################

la_summaries <- growth_groups%>%
  left_join(la_dig_summary, by = "local_authority")%>%
  left_join(la_hhi_summary, by = "local_authority")%>%
  left_join(la_attainment_summary, by = "local_authority")%>%
  left_join(la_demography_summary, by = "local_authority")

print(la_summaries)

################################################################################
#Assumption tests for linearity
################################################################################


#Figure 8: Scatterplot matrix testing for assumptions of linearity
lin_test_df <- data_eda%>%
  pivot_longer(cols = c(average_download_speed_mbit_s, average_upload_speed_mbit_s,
                        hhi ,level_2_attainment_pct, level_3_attainment_pct, net_business),
               names_to = "indicator",
               values_to = "value")%>%
  filter(year != 2015)
  
eda_lin_test_plot <- lin_test_df%>% #----------------FIX THIS
  ggplot(aes(x = value, y = emp_growth_pct))+
  geom_point(alpha = 0.5)+
  geom_smooth(method = "lm", se = FALSE)+
  facet_wrap(~indicator, scales = c("free"))+
  labs(x = "Indicator", y = "Employment Growth (%)")+
  theme_minimal()+
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

print(eda_lin_test_plot)
ggsave("figure8_eda.png", plot = eda_lin_test_plot, path = "outputs/figures")
