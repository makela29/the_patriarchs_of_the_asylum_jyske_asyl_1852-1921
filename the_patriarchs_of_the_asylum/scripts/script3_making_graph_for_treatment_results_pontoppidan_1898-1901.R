##### TREATMENT RESULTS AT JYSKE ASYL UNDER K.B. PONTOPPIDAN, 1898-1901

## LOADING THE DATA:
# Installing the package 
install.packages("tidyverse")

# And activating it
library(tidyverse)

# Using the read_csv() function to load in the data and create an object for it
jyske_asyl_treatment_results <- read_csv("data/jyske_asyl_treatment_results_1878-1945_data.csv")

### DEVELOPMENT OF TREATMENT RESULTS UNDER K.B. PONTOPPIDAN, 1898-1901, LINE-GRAPH
# Visualizing the development with a line-graph

## FILTERING AND ADDING PERCENTAGE COLLUMS
# Filtering the years that K.B. Pontoppidan acted as chief physician 
# The yearly number of outgoing patients' treatment results is turned into percentages
# The years can then be compared while taking the differentiating flow of patients into account
# This is done by creating a new object and using the mutate() function and the formula (part/whole)*100
pct_pontoppidan_treatment_results <- jyske_asyl_treatment_results %>% 
  filter(between(year,1898,1901)) %>% 
  mutate(p_cured_pct=(p_cured/p_total_treated_yr)*100) %>%
  mutate(p_bettered_pct=(p_bettered/p_total_treated_yr)*100) %>% 
  mutate(p_uncured_pct=(p_uncured/p_total_treated_yr)*100) %>% 
  mutate(p_deceased_pct=(p_deceased/p_total_treated_yr)*100)

## PLOTTING THE GRAPH
# Creating a new object and using the ggplot-functions to plot a graph with multiple lines
plot_pontoppidan_treatment_results <- pct_pontoppidan_treatment_results %>% 
  ggplot()+
  geom_line(aes(x=year, y=p_cured_pct, col="Patients cured"))+
  geom_line(aes(x=year, y=p_bettered_pct, col="Patients bettered"))+
  geom_line(aes(x=year, y=p_uncured_pct, col="Patients uncured"))+
  geom_line(aes(x=year, y=p_deceased_pct, col="Patients deceased"))+
  theme_bw()+
  labs(title="Development of treatment results - K.B. Pontopidan, 1898-1901", 
       subtitle= "Fig.5",
       x="Year", 
       y="Percentage of patients",
       colour="Treatment result:")+
  theme(text=element_text(size=24, family ="serif"), 
        legend.position= "bottom")+
  scale_x_continuous(minor_breaks = NULL)+
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1),
                     breaks = seq(0, 18, by = 5), 
                     minor_breaks = seq(0, 18, by = 1),limits = c(0,18))+
  scale_colour_manual(values = c("gold", "forestgreen","black","red"))

# The finale graph:
plot_pontoppidan_treatment_results

# Saving the graph as a png
ggsave("fig5_pontoppidan_development_treatment_results_1898-1901.png", 
       width = 12, height = 10, path = "figures")

### THE DATA FROM THIS SCRIPT IS SAVED IN THE FOLDER "output_data" 