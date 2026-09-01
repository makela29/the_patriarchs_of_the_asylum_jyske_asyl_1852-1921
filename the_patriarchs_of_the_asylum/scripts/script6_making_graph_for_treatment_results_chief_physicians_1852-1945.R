##### TREATMENT RESULTS AT JYSKE ASYL UNDER DIFFERENT CHIEF PHYSICIANS, 1878-1945

## LOADING THE DATA:
# Installing the package 
install.packages("tidyverse")

# And activating it
library(tidyverse)

# Using the read_csv() function to load in the data and create an object for it
pct_outgoing_selmer_treatment_results<- read.csv2("data/selmer_treatment_results_1852-1877_data.csv")

### COMPARING THE TREATMENT RESULTS FOR THE CHIEF PYSICIANS, 1852-1945, BAR GRAPH
# Visualizing the development with a line-graph
### ADDING ROWS TO THE DATASET 
# Adding the other chief physicians' average treatment results of outgoing patients 

# Using the read_csv() function to load in the data and create an object for it
jyske_asyl_treatment_results <- read_csv("data/jyske_asyl_treatment_results_1878-1945_data.csv")

## FILTERING AND ADDING PERCENTAGE COLLUMS
# Filtering the years for the acting chief physicians
# Transforming treatment results of cured and bettered patients into percentages 
# Using outgoing patients as 100% in order to match the data from Selmer
# This is done by creating a new object and using the mutate() function and the formula (part/whole)*100
pct_outgoing_holm_treatment_results <-jyske_asyl_treatment_results %>% 
  mutate(p_total_outgoing=p_cured+p_bettered+p_uncured+p_deceased+
           p_not_men_ill+p_discharged_trial) %>%
  mutate(p_cured_pct=(p_cured/p_total_outgoing)*100) %>%
  mutate(p_bettered_pct=(p_bettered/p_total_outgoing)*100) %>% 
  filter(year<1899)

pct_outgoing_pontoppidan_treatment_results <-jyske_asyl_treatment_results %>% 
  mutate(p_total_outgoing=p_cured+p_bettered+p_uncured+p_deceased+
           p_not_men_ill+p_discharged_trial) %>%
  mutate(p_cured_pct=(p_cured/p_total_outgoing)*100) %>%
  mutate(p_bettered_pct=(p_bettered/p_total_outgoing)*100) %>% 
  filter(between(year,1898,1901))
  
pct_outgoing_hallager_treatment_results <-jyske_asyl_treatment_results %>% 
  mutate(p_total_outgoing=p_cured+p_bettered+p_uncured+p_deceased+
           p_not_men_ill+p_discharged_trial) %>%
  mutate(p_cured_pct=(p_cured/p_total_outgoing)*100) %>%
  mutate(p_bettered_pct=(p_bettered/p_total_outgoing)*100) %>% 
  filter(between(year,1901,1921))
  
pct_outgoing_jacobsen_treatment_results <-jyske_asyl_treatment_results %>% 
  mutate(p_total_outgoing=p_cured+p_bettered+p_uncured+p_deceased+
           p_not_men_ill+p_discharged_trial) %>%
  mutate(p_cured_pct=(p_cured/p_total_outgoing)*100) %>%
  mutate(p_bettered_pct=(p_bettered/p_total_outgoing)*100) %>% 
  filter(year>1920)

## FINDING THE AVERAGE 
# Using the round() and mean() functions to find the average 
round(mean(pct_outgoing_holm_treatment_results$p_cured_pct),2)
# =36.32
round(mean(pct_outgoing_holm_treatment_results$p_bettered_pct),2)
# =17.05

round(mean(pct_outgoing_pontoppidan_treatment_results$p_cured_pct),2)
# =27.79
round(mean(pct_outgoing_pontoppidan_treatment_results$p_bettered_pct),2)
# =28.44

round(mean(pct_outgoing_hallager_treatment_results$p_cured_pct),2)
# =26.9
round(mean(pct_outgoing_hallager_treatment_results$p_bettered_pct),2)
# =23.87

round(mean(pct_outgoing_jacobsen_treatment_results$p_cured_pct),2)
# =24.74
round(mean(pct_outgoing_jacobsen_treatment_results$p_bettered_pct),2)
# =40.33


## ADDING NEW ROWS
# Creating a new object and dding the new rows for the other chief physicians
pct_chiefphys_treatment_results <- pct_outgoing_selmer_treatment_results
pct_chiefphys_treatment_results[nrow(pct_chiefphys_treatment_results) + 1, ] <- c("cured", 36.32 ,"holm")
pct_chiefphys_treatment_results[nrow(pct_chiefphys_treatment_results) + 1, ] <- c("bettered", 17.05 ,"holm")
pct_chiefphys_treatment_results[nrow(pct_chiefphys_treatment_results) + 1, ] <- c("cured", 27.79 ,"pontoppidan")
pct_chiefphys_treatment_results[nrow(pct_chiefphys_treatment_results) + 1, ] <- c("bettered", 28.44 ,"pontoppidan")
pct_chiefphys_treatment_results[nrow(pct_chiefphys_treatment_results) + 1, ] <- c("cured", 26.9 ,"hallager")
pct_chiefphys_treatment_results[nrow(pct_chiefphys_treatment_results) + 1, ] <- c("bettered", 23.87 ,"hallager")
pct_chiefphys_treatment_results[nrow(pct_chiefphys_treatment_results) + 1, ] <- c("cured", 24.74 ,"jacobsen")
pct_chiefphys_treatment_results[nrow(pct_chiefphys_treatment_results) + 1, ] <- c("bettered", 40.33 ,"jacobsen")

## INSPECTING THE DATA
glimpse(pct_chiefphys_treatment_results)
# Changing the percentage of patients to a numeric column 
pct_chiefphys_treatment_results$pct_p <- as.numeric(pct_chiefphys_treatment_results$pct_p)
glimpse(pct_chiefphys_treatment_results)

## PLOTTING THE GRAPH
# Creating a new object and using the ggplot-functions to plot a bar-graph 
plot_chiefphys_treatment_results <- pct_chiefphys_treatment_results %>% 
  ggplot(aes(x=chief_phys, y=pct_p, fill = treatment_results))+
  geom_bar(stat="identity")+
  theme_bw()+
  labs(title="Comparing possitive treatment results, chief physicians, 1852-1945", 
       subtitle= "Fig.8",
       x="Chief physician", 
       y="Percentage of patients",
       fill="Treatment result:") +
  theme(text=element_text(size=24, family ="serif"),legend.position= "bottom")+
  geom_text(aes(label = pct_p), hjust = 0.5, vjust = 3, position = "stack",
            size=10, family= "serif")+
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1),
                     breaks = seq(0, 80, by = 5), 
                     minor_breaks = seq(0, 80, by = 1))+
  scale_x_discrete(labels=c("selmer" = "Selmer 1852-1877", "holm" = "Holm 1878-1898", "pontoppidan" = 
                            "Pontoppidan 1898-1901","hallager" = "Hallager 1901-1921",
                            "jacobsen" = "Jacobsen 1921-1945"),
                   limits = c("selmer","holm", "pontoppidan","hallager","jacobsen"))+
  scale_fill_manual(values = c("gold", "forestgreen"))

# The finale graph:
plot_chiefphys_treatment_results

# Saving the graph as a png
ggsave("fig8_chief_pysicians_treatment_results_1852-1945.png", 
       width = 15, height = 10, path = "figures")

### THE DATA FROM THIS SCRIPT IS SAVED IN THE FOLDER "output_data"