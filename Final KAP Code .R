
install.packages("broom")
install.packages("kableExtra")
install.packages("magrittr")

library(kableExtra)
library(dplyr)
library(knitr)
library(magrittr)
library(readxl)
library(ggplot2)
library(stats)

#data input
main_data <- read_excel("Coded data - Copy.xlsx")
View(main_data)


#missing value
sum(is.na(main_data))



#data labeling demographic data


main_data$How_many_years_working_in_a_hospital <- factor(main_data$How_many_years_working_in_a_hospital ,
                                                      levels = c("I am on attachment","I am a trainee in medicine (internship)", "less than one year","1 to 3 years","4 to 6 years","7 years and more"),
                                                      ordered = TRUE)


main_data$How_many_years_working_in_a_hospital <- as.numeric(main_data$How_many_years_working_in_a_hospital )


main_data$How_many_years_working_in_a_hospital[is.na(main_data$How_many_years_working_in_a_hospital)] <- 
  median(main_data$How_many_years_working_in_a_hospital, na.rm = TRUE)



main_data$Department<-factor(main_data$Department,
                             levels = c("Medicine/Emergency",
                                        "Obstetrics and Gynaecology",
                                        "Outpatients",
                                        "Pharmacy",
                                        "Surgery",
                                        "Others"),
                             ordered = TRUE)

main_data$Department<-as.numeric(main_data$Department)
main_data$Department[is.na(main_data$Department)]<- median(main_data$Department,na.rm = TRUE)

View(main_data)

main_data$Designation...6<-factor(main_data$Designation...6,
                                  levels = c("CO","Consultant","MO","Pharmacist"),
                                  ordered = TRUE)
main_data$Designation...6<- as.numeric(main_data$Designation...6)

main_data$Designation...6[is.na(main_data$Designation...6)]<-median(main_data$Designation...6, na.rm = TRUE)


View(main_data)

#cut method for knowlege_percentage and Attitude_percentage

main_data$Knowladge_parcentage[is.na(main_data$Knowladge_parcentage)]<- mean(main_data$Knowladge_parcentage,na.rm = TRUE)
main_data$Practice_percentage[is.na(main_data$Practice_percentage)]<- mean(main_data$Practice_percentage,na.rm = TRUE)
main_data$Attitude_percentage[is.na(main_data$Attitude_percentage)]<- mean(main_data$Attitude_percentage,na.rm = TRUE)


main_data$Attitude_percentage<- cut(main_data$Attitude_percentage,
                                    breaks = c(-Inf, 51, 79, Inf),
                                    labels = c("Poor","Average","Good"))



main_data$Knowladge_parcentage <- cut(main_data$Knowladge_parcentage,
                                      breaks = c(-Inf, 51, 79, Inf),
                                      labels = c("Poor","Average","Good"))

main_data$Practice_percentage <- cut(main_data$Practice_percentage,
                                     breaks = c(-Inf, 51, 79, Inf),
                                     labels = c("Poor","Average","Good"))

View(main_data)
names(main_data)
str(main_data)



#chi-square test 
#Convert chr to factor 

main_data$Attitude_percentage <- factor(main_data$Attitude_percentage)
main_data$Practice_percentage <- factor(main_data$Practice_percentage )
main_data$Knowladge_parcentage<-factor(main_data$Knowladge_parcentage)

main_data$How_many_years_working_in_a_hospital<-factor(main_data$How_many_years_working_in_a_hospital)
main_data$Department<-factor(main_data$Department)
main_data$Designation...6<-factor(main_data$Designation...6)

str(main_data)

# Conduct Chi-square tests to assess associations between demographic factors and KAP outcomes.

demographic_var <- c("How_many_years_working_in_a_hospital","Department","Designation...6")
KAP_var <- c("Attitude_percentage","Knowladge_parcentage","Practice_percentage")

#
results <- data.frame(demographic_var = character(),
                      KAP_var = character(),
                      KW_Statistic = numeric(),
                      DF = numeric(),
                      P_Value = numeric(),
                      stringsAsFactors = FALSE
)

# Loop through demographic and KAP variables

for (demo in demographic_var) {
  for (kap in KAP_var) {
    if (demo %in% colnames(main_data) && kap %in% colnames(main_data)) {
      
      table_data <- table(main_data[[demo]], main_data[[kap]])
      if (nrow(table_data) > 1 && ncol(table_data) > 1) {
        test_result <- chisq.test(table_data, simulate.p.value = TRUE)
        df_value <- (nrow(table_data) - 1) * (ncol(table_data) - 1)
        
        results <- rbind(results, data.frame(
          demographic_var = demo,
          KA_var = kap,
          Chi_Square = round(test_result$statistic, 3),
          DF = df_value,  
          P_Value = round(test_result$p.value, 3)
        ))
      }
    }
  }
}
print(results)


kableExtra::kbl(results, 
                caption = "Chi-Square Test Results for Demographic Variables vs. KAP Outcomes") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                            full_width = FALSE, 
                            font_size = 14) %>%
  kableExtra::column_spec(1, bold = TRUE) %>%
  kableExtra::column_spec(6, color = ifelse(results$P_Value < 0.05, "red", "black"))



#Perform t-tests or ANOVA (as appropriate) to compare attitudes across different practitioner groups.

main_data$Designation...6 <- as.factor(main_data$Designation...6)
main_data$Attitude_percentage <- as.numeric(main_data$Attitude_percentage)

anova_model <- aov(Attitude_percentage ~ Designation...6, data = main_data)
summary(anova_model)

# Practioner groups


library(broom)
library(kableExtra)

colnames(main_data)[colnames(main_data) == "On_average_for_every_10_patients_how_many_do_you_prescribed_antibiotics_to?"] <- "Avg_10_Patient_Prescription"
main_data$Avg_10_Patient_Prescription <-as.factor(main_data$Avg_10_Patient_Prescription)
main_data$`To_whom_do_you_prescribe?`<- as.factor(main_data$`To_whom_do_you_prescribe?`)
main_data$`Do_you_follow_any_antibiotic_prescription_guidelines?`<- as.factor(main_data$`Do_you_follow_any_antibiotic_prescription_guidelines?`)


model_1 <- aov(Attitude_percentage ~ Avg_10_Patient_Prescription , data = main_data)

summary(model_1)

model_2 <- aov(Attitude_percentage ~ `To_whom_do_you_prescribe?` , data = main_data)

summary(model_2)

model_3 <- aov(Attitude_percentage ~ `Do_you_follow_any_antibiotic_prescription_guidelines?` , data = main_data)

summary(model_3)

#combine table

anova_results_1 <- tidy(anova_model)
anova_results_2 <- tidy(model_1)
anova_results_3 <- tidy(model_2)
anova_results_4 <- tidy(model_3)

anova_results <- bind_rows(
  anova_results_1 %>% mutate(Model = "anova_model"),
  anova_results_2 %>% mutate(Model = "Model 1"),
  anova_results_2 %>% mutate(Model = "Model 2"),
  anova_results_4 %>% mutate(Model = "Model 3")
)

anova_results <- anova_results %>%
  mutate(p.value = ifelse(is.na(p.value), 1, p.value))

         
kableExtra::kbl(anova_results, 
                caption = "ANOVA Test Results for Different Models") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                            full_width = FALSE, 
                            font_size = 14) %>%
  kableExtra::column_spec(1, bold = TRUE) %>%
  kableExtra::column_spec(5, color = ifelse(anova_results$p.value < 0.05, "red", "black"))



#visualization of data
library(ggplot2)
#Bar Plot For Attitude
main_data3 <- main_data |> 
  mutate(Attitude_status = case_when(
    Attitude_percentage == 1 ~ "Poor",
    Attitude_percentage == 2 ~ "Average",
    Attitude_percentage == 3 ~ "Good"
  ))

ggplot(main_data3, aes(x = Attitude_status)) +
  geom_bar(position = "dodge", fill = "#1F77B4", width = 0.3) +  # Professional Blue Color
  labs(
    title = "Distribution of Attitude on Antibiotic Prescribing & Resistance",
    x = "Attitude Status",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +  # Base font size বড় করা
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),  # Title Centered & Bold
    axis.text = element_text(size = 12),  # Axis Labels Bigger
    axis.title = element_text(face = "bold"),  # Axis Titles Bold
    panel.grid.major.x = element_blank(),  # X-axis Grid Line Remove
    panel.grid.minor = element_blank()  # Minor Grid Remove
  ) +
  ylim(0, max(table(main_data3$Attitude_status)) + 5)  # Height Adjusted



#Knowledge Section
ggplot(main_data3, aes(x = `Score Level`)) +
  geom_bar(position = "dodge", fill = "#1F77B4", width = 0.3) +  # Professional Blue Color
  labs(
    title = "Distribution of Knowledge on Antibiotic Prescribing & Resistance",
    x = "Knowledge Status",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +  # Base font size adjustment
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),  # Title Centered & Bold
    axis.text = element_text(size = 12),  # Axis Labels Bigger
    axis.title = element_text(face = "bold"),  # Axis Titles Bold
    panel.grid.major.x = element_blank(),  # X-axis Grid Line Remove
    panel.grid.minor = element_blank()  # Minor Grid Remove
  ) +
  ylim(0, max(table(main_data3$`Score Level`)) + 5)  # Height Adjusted

#Practice Section 
install.packages("RColorBrewer", dependencies = TRUE)  
library(ggplot2)

library(RColorBrewer)  

#Select coloum
main_data <- main_data %>% 
  select(Practice_Percentage, Designation) %>% 
  mutate(Designation...7 = as.factor(Designation))
#Create Group
grouped_data <- main_data %>%
  group_by(Practice_Percentage, Designation) %>%
  summarise(count = n(), .groups = "drop")

#Create Heat map
ggplot(grouped_data, aes(x = Practice_Percentage, y = Designation, fill = count)) +
  geom_tile() +  # Create the heatmap tiles
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Smooth color gradient based on count
  labs(
    x = "Practice Percentage (Score)",  # X-axis label
    y = "Designation",  # Y-axis label
    fill = "Count",  # Legend label
    title = "Heatmap of Practice Percentage vs. Designation"  # Title without subtitle
  ) +
  theme_minimal() +  # Clean minimal theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotate x-axis labels for readability
    axis.text.y = element_text(size = 12),  # Increase font size for y-axis labels
    axis.title = element_text(size = 14),  # Increase font size for axis titles
    plot.title = element_text(size = 16, face = "bold"),  # Make the plot title bold and larger
    legend.title = element_text(size = 12),  # Legend title size
    legend.text = element_text(size = 10),  # Legend text size
    panel.grid = element_blank(),  # Remove gridlines for a cleaner look
    plot.margin = margin(10, 10, 10, 10)  # Add margin around the plot for spacing
  )



