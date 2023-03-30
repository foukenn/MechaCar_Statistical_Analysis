#import dplyr
library(dplyr)

#Linear Regression to Predict MPG

#reading csv in
mechacarMPG_df <- read.csv(file='MechaCar_mpg.csv', check.names = F, stringsAsFactors = F)

#Regression
lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD, data = mechacarMPG_df)

#P-Value and R-Squared
summary(lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD, data = mechacarMPG_df))


#Suspension Coil Lot Analysis

#reading csv in
suspension_df <- read.csv(file = 'Suspension_Coil.csv', check.names = F, stringsAsFactors = F)

#summary frame
total_summary <- suspension_df %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI))

#create summaries for each lot
lots_summary <- suspension_df %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI), .groups = 'keep')

# T-tests 
t.test(suspension_df$PSI, mu=1500)

t.test(subset(suspension_df,Manufacturing_Lot=="Lot1")$PSI, mu = 1500)
t.test(subset(suspension_df,Manufacturing_Lot=="Lot2")$PSI, mu = 1500)
t.test(subset(suspension_df,Manufacturing_Lot=="Lot3")$PSI, mu = 1500)
