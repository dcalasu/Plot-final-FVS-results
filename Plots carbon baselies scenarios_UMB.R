#d <- read_excel("/Users/danic/Downloads/OneFlux/All_sites.xlsx")
library(readxl)
d <- read_excel("C:/Users/danic/Downloads/Combined_baselines_ASites.xlsx")
d <- read_excel("C:/Users/danic/OneDrive - Indiana University/Research/Data collected/Baselines comparaisons/MMSF_2023/Baselines_cores_FVS.xlsx")
d <- read_excel("C:/Users/danic/OneDrive - Indiana University/Research/Data collected/Baselines comparaisons/UMB/Carbon baselines_FVS_Cores_UMB.xlsx")
### Plot NEE
d$`NEE (MT C/Ha year)_MMS` <- as.numeric(d$`NEE (MT C/Ha year)_MMS`)
d$`NEE (MT C/Ha year)_Ha1` <- as.numeric(d$`NEE (MT C/Ha year)_Ha1`)
d$`NEE (MT C/Ha year)_Moz` <- as.numeric(d$`NEE (MT C/Ha year)_Moz`)
d$`Tree core (ton C/ha)` <- as.numeric(d$`Tree core (ton C/ha)`)
d$Avg_MMS <- as.numeric(d$Avg_MMS)
d$avg_core <- as.numeric(d$avg_core)

d$`NEE (MT C/Ha year)_MMS` <- abs(d$`NEE (MT C/Ha year)_MMS`)
d$`NEE (MT C/Ha year)_Ha1` <- abs(d$`NEE (MT C/Ha year)_Ha1`)
d$`NEE (MT C/Ha year)_Moz` <- abs(d$`NEE (MT C/Ha year)_Moz`)
d$Avg_MMS <- abs(d$Avg_MMS)
d$avg_core <- abs(d$avg_core)

####BASIC PLOT
plot(d$Year, d$`NEE (MT C/Ha year)_Ha1`)
plot(d$Year, d$`NEE (MT C/Ha year)_MMS`, type = "l", ylim = c(0, 6), xlim = c(1998,2017))
lines(d$Year[!is.na(d$`Tree core (ton C/ha)`)], d$`Tree core (ton C/ha)`[!is.na(d$`Tree core (ton C/ha)`)])
lines (new$Year, new$Crate)

####ggplot interannual variability
df <- data.frame(
  Year = d$Year...1,
  Avg_MMS = d$`Crate_FVS (Cton/CA)`,
  avg_core = d$`Stand total (ton C/ha)`
)

df <- data.frame(
  Year = d$Year,
  Avg_MMS = d$Crate_MMSFVS,
  avg_core = d$Crate_MMSFcores,
  NEE = d$NEE_MMSF
)




df <- data.frame(
  Year = d$Year,
  Avg_MMS = d$Avg_MMS,
  `NEE (MT C/Ha year)_MMSF` = d$`NEE (MT C/Ha year)_MMS`,
  `Tree core (ton C/ha)` = d$`Tree core (ton C/ha)`,
  avg_core = d$avg_core
)

df <- data.frame(
  Year = d$Year,
  `NEE (MT C/Ha year)_Ha1` = d$`NEE (MT C/Ha year)_Ha1`
)

df <- data.frame(
  Year = d$Year,
  `NEE (MT C/Ha year)_Moz` = d$`NEE (MT C/Ha year)_Moz`
)
# Create the line plot
####MOZ
d$`Crate_FVS (Cton/CA)` <- as.numeric(d$`Crate_FVS (Cton/CA)`)
ggplot(df) +
  geom_line(aes(x = d$Year...1, y = d$`Stand total (ton C/ha)`, color = "Tree cores (ton C/ha)")) +
  geom_line(aes(x = d$Year...3, y = d$`Crate_FVS (Cton/CA)`, color = "FVS projection")) +
  ylim(0, 3) +
  xlim(1927, 2025) +
  labs(x = "Year", y = "Change in biomass (MT C/Ha year)", color = "Variable") +
  scale_color_manual(values = c("Tree cores (ton C/ha)" = "green4", "FVS projection" = "red4")) +
  ggtitle("MMSF Change in Biomass")

###UMB
d$Crate_FVS <- as.numeric(d$Crate_FVS)
ggplot(df) +
  geom_line(aes(x = d$Year...4, y = d$Crate_Cores, color = "Tree cores (ton C/ha)")) +
  geom_line(aes(x = d$Year...2, y = d$Crate_FVS, color = "FVS projection")) +
  ylim(0, 3) +
  xlim(1998, 2100) +
  labs(x = "Year", y = "Change in biomass (MT C/Ha year)", color = "Variable") +
  scale_color_manual(values = c("Tree cores (ton C/ha)" = "green4", "FVS projection" = "red4")) +
  ggtitle("MMSF Change in Biomass")


ggplot(df) +
  geom_line(aes(x = Year, y = Avg_MMS, color = "5 year avg NEE (MT C/Ha year)_MMS")) +
  geom_line(aes(x = Year, y = avg_core, color = "5 year avg Tree core (ton C/ha)")) +
  geom_line(data = new, aes(x = Year, y = Crate, color = "FVS projection")) +
  ylim(0, 6) +
  xlim(1998, 2017) +
  labs(x = "Year", y = "Biomass Carbon uptake (MT C/Ha year)", color = "Variable") +
  scale_color_manual(values = c("5 year avg NEE (MT C/Ha year)_MMS" = "blue", "5 year avg Tree core (ton C/ha)" = "red", "FVS projection" = "green")) +
  ggtitle("MMSF Carbon baselines 5 year period")



ggplot(df) +
  geom_line(aes(x = Year, y = NEE..MT.C.Ha.year._MMSF, color = "NEE (MT C/Ha year)_MMS")) +
  geom_line(aes(x = Year, y = Tree.core..ton.C.ha., color = "Tree core (ton C/ha)")) +
  geom_line(data = new, aes(x = Year, y = Crate, color = "FVS projection")) +
  ylim(0, 6) +
  xlim(1998, 2017) +
  labs(x = "Year", y = "Biomass Carbon uptake (MT C/Ha year)", color = "Variable") +
  scale_color_manual(values = c("NEE (MT C/Ha year)_MMS" = "blue", "Tree core (ton C/ha)" = "red", "FVS projection" = "green")) +
  ggtitle("MMSF Carbon baselines")

ggplot(df) +
  geom_line(aes(x = Year, y = NEE..MT.C.Ha.year._MMS, color = "NEE (MT C/Ha year)_MMS")) +
  geom_line(aes(x = Year, y = Tree.core..ton.C.ha., color = "Tree core (ton C/ha)")) +
  geom_line(data = new, aes(x = Year, y = Crate, color = "FVS projection")) +
  geom_smooth(aes(x = Year, y = NEE..MT.C.Ha.year._MMS, color = "NEE (MT C/Ha year)_MMS"), method = "lm", se = FALSE) +
  geom_smooth(aes(x = Year, y = Tree.core..ton.C.ha., color = "Tree core (ton C/ha)"), method = "lm", se = FALSE) +
  #geom_smooth(data = new, aes(x = Year, y = Crate, color = "FVS projection"), method = "lm", se = FALSE) +
  ylim(0, 6) +
  xlim(1998, 2017) +
  labs(x = "Year", y = "Carbon uptake (MT C/Ha year)", color = "Variable") +
  scale_color_manual(values = c("NEE (MT C/Ha year)_MMS" = "blue", "Tree core (ton C/ha)" = "red", "FVS projection" = "green")) +
  ggtitle("MMSF Carbon baselines")



ggplot(df) +
  geom_line(aes(x = Year, y = NEE..MT.C.Ha.year._Ha1, color = "NEE (MT C/Ha year)_Ha1")) +
  geom_line(data = new, aes(x = Year, y = Crate, color = "FVS projection")) +
  ylim(0, 7) +
  xlim(1996, 2017) +
  labs(x = "Year", y = "Carbon uptake (MT C/Ha year)", color = "Variable") +
  scale_color_manual(values = c("NEE (MT C/Ha year)_Ha1" = "blue", "FVS projection" = "green")) +
  ggtitle("Carbon baselines US_Ha1")
#Moz
ggplot(df) +
  geom_line(aes(x = Year, y = NEE..MT.C.Ha.year._Moz, color = "NEE (MT C/Ha year)_Moz")) +
  geom_line(data = new, aes(x = Year, y = Crate, color = "FVS projection")) +
  geom_smooth(aes(x = Year, y = NEE..MT.C.Ha.year._Moz, color = "NEE (MT C/Ha year)_Moz"), method = "lm", se = FALSE) +
  ylim(0, 7) +
  xlim(2005, 2017) +
  labs(x = "Year", y = "Carbon uptake (MT C/Ha year)", color = "Variable") +
  scale_color_manual(values = c("NEE (MT C/Ha year)_Moz" = "blue", "FVS projection" = "green")) +
  ggtitle("Carbon baselines US_Moz")


#####
# Create a data frame with x and y values
df <- data.frame(x = NEE..MT.C.Ha.year._MMS, y = Tree.core..ton.C.ha.)

# Create a ggplot with scatter plot and diagonal reference line
ggplot(df, aes(x = NEE..MT.C.Ha.year._MMS, y = Tree.core..ton.C.ha.)) +
  geom_point(mapping = aes(x = NEE..MT.C.Ha.year._MMS, y = Tree.core..ton.C.ha.))+
  geom_smooth(mapping = aes(x = NEE..MT.C.Ha.year._MMS, y = Tree.core..ton.C.ha.))+
  #geom_point() +
  #geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  xlim(0, 6) +
  ylim(0, 2) +
  labs(x = "NEE MMSF", y = "Cores") +
  ggtitle("NEE vs Tree cores ")

# Create a ggplot scatter plot
df <- data.frame(x = NEE..MT.C.Ha.year._MMS, y = Tree.core..ton.C.ha.)
ggplot(df_c2, aes(x = NEE..MT.C.Ha.year._MMS, y = Tree.core..ton.C.ha.)) +
  geom_point() +
  geom_smooth()+
  xlim(0, 6) +
  ylim(0, 2) +
  labs(x = "NEE MMSF", y = "Cores") +
  ggtitle("NEE vs Tree cores")












plot(d$Year, d$Avg_MMS, type = "l", ylim = c(0, 6), xlim = c(1998,2017))
lines(d$Year[!is.na(d$avg_core)], d$avg_core[!is.na(d$avg_core)])


### stacked chart

library(ggplot2)

# Prepare the data for the stacked bar chart
df_stacked <- data.frame(
  Year = d$Year,
  Avg_MMS = d$Avg_MMS,
  avg_core = d$avg_core
)

# Reshape the data into long format
df_stacked <- tidyr::pivot_longer(df_stacked, cols = c(Avg_MMS, avg_core), names_to = "Category", values_to = "Value")

# Create the stacked bar chart
ggplot(df_stacked, aes(x = Year, y = Value, fill = Category)) +
  geom_bar(stat = "identity") +
  ylim(0, 6) +
  xlim(1998, 2017) +
  labs(x = "Year", y = "Value", fill = "Category") +
  ggtitle("Stacked Bar Chart")


#####grouped chart
# Create a new data frame for the grouped bar chart
df_grouped <- data.frame(
  Year = d$Year,
  Flux = d$Avg_MMS,
  avg_core = d$avg_core
)

# Reshape the data into long format
df_grouped <- tidyr::pivot_longer(df_grouped, cols = c(Avg_MMS, avg_core), names_to = "Category", values_to = "Value")

# Create the grouped bar chart
ggplot(df_grouped, aes(x = Year, y = Value, fill = Category)) +
  geom_col(position = "dodge") +
  ylim(0,5.5) +
  xlim(1998, 2015) +
  labs(x = "Year", y = "Value", fill = "Category") +
  ggtitle("Grouped Bar Chart")

#####

View (d)




#####

par(mfrow = c(1, 2))
plot(cars, main="Speed vs Distance", col="red")
plot(mtcars$mpg, mtcars$hp, main="HP vs MPG", col="blue")

#####

