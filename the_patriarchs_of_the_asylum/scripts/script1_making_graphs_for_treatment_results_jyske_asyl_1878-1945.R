##### TREATMENT RESULTS AT JYSKE ASYL, 1878-1945

## LOADING THE DATA:
# Installing the package 
install.packages("tidyverse")

# And activating it
library(tidyverse)

# Using the read_csv() function to load in the data and create an object for it
jyske_asyl_treatment_results <- read_csv("data/jyske_asyl_treatment_results_1878-1945_data.csv")

### OVERALL TREND OF TREATMENT RESULTS AT JYSKE ASYL, 1878-1945, LINEAR REGRESSION
# Visualizing the overall trend with a linear-regression-graph 

## ADDING PERCENTAGE COLLUMS
# The yearly number of outgoing patients' treatment results is turned into percentages
# The years can then be compared while taking the differentiating flow of patients into account
# This is done by creating a new object and using the mutate() function and the formula (part/whole)*100
pct_jyske_asyl_treatment_results <- jyske_asyl_treatment_results %>% 
  mutate(p_cured_pct=(p_cured/p_total_treated_yr)*100) %>%
  mutate(p_bettered_pct=(p_bettered/p_total_treated_yr)*100) %>% 
  mutate(p_uncured_pct=(p_uncured/p_total_treated_yr)*100) %>% 
  mutate(p_deceased_pct=(p_deceased/p_total_treated_yr)*100)

## PLOTTING THE GRAPH
# Creating a new object and using the ggplot-functions to plot a graph with linear regression
plot_lin_reg_jyske_asyl_treatment_results<- pct_jyske_asyl_treatment_results %>% 
  ggplot()+
  geom_point(aes(x=year, y=p_cured_pct, col="Patients cured"))+
  geom_point(aes(x=year, y=p_bettered_pct, col="Patients bettered"))+
  geom_point(aes(x=year, y=p_uncured_pct, col="Patients uncured"))+
  geom_point(aes(x=year, y=p_deceased_pct, col="Patients deceased"))+
  geom_smooth(aes(x=year, y=p_cured_pct, col="Patients cured"))+
  geom_smooth(aes(x=year, y=p_bettered_pct, col="Patients bettered"))+
  geom_smooth(aes(x=year, y=p_uncured_pct, col="Patients uncured"))+
  geom_smooth(aes(x=year, y=p_deceased_pct, col="Patients deceased"))+
  theme_bw()+
  labs(title="Treatment trends at Jyske Asyl - historical overview, 1878-1945",
       subtitle= "Fig.1",
       x="Year", 
       y="Percentage of patients",
       colour="Treatment result:")+
  theme(text=element_text(size=24, family ="serif"), 
        legend.position= "bottom")+
  scale_x_continuous( breaks = seq(1875, 1945, by = 5), 
                      minor_breaks = seq(1878, 1945, by = 1))+
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1),
                     breaks = seq(0, 18, by = 5), 
                     minor_breaks = seq(0, 18, by = 1),limits = c(0,18))+
  scale_colour_manual(values = c("gold", "forestgreen","black","red"))

# The final graph:
plot_lin_reg_jyske_asyl_treatment_results

# Saving the graph as a png file
ggsave("fig1_linear_regression_jyske_asyl_development_treatment_results_1878-1945.png", 
       width = 20, height = 7, path = "figures")

### DEVELOPMENT OF TREATMENT RESULTS AT JYSKE ASYL, 1878-1945, LINE-GRAPH
# Visualizing the development with a line-graph

## PLOTTING THE GRAPH
# Creating a new object and using the ggplot-functions to plot a graph with multiple lines
plot_jyske_asyl_treatment_results <- pct_jyske_asyl_treatment_results %>% 
  ggplot()+
  geom_line(aes(x=year, y=p_cured_pct, col="Patients cured"))+
  geom_line(aes(x=year, y=p_bettered_pct, col="Patients bettered"))+
  geom_line(aes(x=year, y=p_uncured_pct, col="Patients uncured"))+
  geom_line(aes(x=year, y=p_deceased_pct, col="Patients deceased"))+
  theme_bw()+
  labs(title="Development of treatment results at Jyske Asyl, 1878-1945", 
       subtitle= "Fig.2",
       x="Year", 
       y="Percentage of patients",
       colour="Treatment result:")+
  theme(text=element_text(size=24, family ="serif"), 
        legend.position= "bottom")+
  scale_x_continuous( breaks = seq(1875, 1945, by = 5), 
                      minor_breaks = seq(1878, 1945, by = 1))+
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1),
                     breaks = seq(0, 18, by = 5), 
                     minor_breaks = seq(0, 18, by = 1),limits = c(0,18))+
  scale_colour_manual(values = c("gold", "forestgreen","black","red"))

# The final graph:
plot_jyske_asyl_treatment_results

# Saving the graph as a png file
ggsave("fig2_jyske_asyl_development_treatment_results_1878-1945.png", 
       width = 20, height = 7, path = "figures")

### ????? OUTGOING AND REMAINING PATIENTS AT JYSKE ASYL, 1878-1945, LINE-GRAPH
# Visualizing the development with a line-graph

## ADDING COLLUMS
# Creating a column of percentage of remaining patients
# This is done by creating a new object and using the mutate() function and the formula (part/whole)*100
pct_remaining_outgoing_comparison <- pct_jyske_asyl_treatment_results %>% 
  mutate(p_total_outgoing=p_cured+p_bettered+p_uncured+p_deceased+
           p_not_men_ill+p_discharged_trial) %>% 
  mutate(p_total_remaining=p_total_treated_yr-p_total_outgoing) %>% 
  mutate(p_total_remaining_pct=(p_total_remaining/p_total_treated_yr)*100)

## PLOTTING THE GRAPH
# Creating a new object and using the ggplot-functions to plot a graph with multiple lines
plot_remaining_outgoing_comparison <- pct_remaining_outgoing_comparison %>% 
  ggplot()+
  geom_line(aes(x=year, y=p_cured_pct, col="Patients cured"))+
  geom_line(aes(x=year, y=p_bettered_pct, col="Patients bettered"))+
  geom_line(aes(x=year, y=p_uncured_pct, col="Patients uncured"))+
  geom_line(aes(x=year, y=p_deceased_pct, col="Patients deceased"))+
  geom_line(aes(x=year, y=p_total_remaining_pct, col="Patients staying"))+
  theme_bw()+
  labs(title="Development of remaining vs. outgoing patients, 1878-1945", 
       subtitle= "Fig.3",
       x="Year", 
       y="Percentage of patients",
       colour="?")+
  theme(text=element_text(size=24, family ="serif"), 
        legend.position= "bottom")+
  scale_x_continuous( breaks = seq(1875, 1945, by = 5), 
                      minor_breaks = seq(1878, 1945, by = 1))+
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1),
                     breaks = seq(0, 100, by = 5),limits = c(0,95),
                     minor_breaks = NULL)+
  scale_colour_manual(values = c("gold", "forestgreen","black","blue","red"))

# The final graph:
plot_remaining_outgoing_comparison

# Saving the graph as a png file
ggsave("fig3_jyske_asyl_development_remaining_outgoing_1878-1945.png", 
       width = 20, height = 7, path = "figures")

### THE DATA FROM THIS SCRIPT IS SAVED IN THE FOLDER "output_data" 