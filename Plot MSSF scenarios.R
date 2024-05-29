F2 <- read.csv("UMB_2023_FVS_out_Table_DefaultSI.csv")
F3_oldData <- read.csv("UMB_2023_FVS_out_Table_LocalSI.csv")
MOZ_2023_FVS_out_Table_LocalSIupdate
# Set up the plotting area
US_Ha1$Year <- as.numeric(US_Ha1$Year)
F3_oldData$Year <- as.numeric(table$Year)

# Set up the plot with appropriate limits and labels
plot(F3_oldData$Year, F3_oldData$Crate, type = "n", xlim = c(min(F3_oldData$Year), max(F3_oldData$Year)), ylim = c(0, 4),
     main = "Change in carbon uptake MMSF", xlab = "Year", ylab = "Change in carbon uptake (MT C/Ha/Year)")
# Plot each dataset
lines(F3_oldData$Year, F3_oldData$Crate, col = "blue", lwd = 2)
lines(F2$Year, F2$Crate, col = "red", lwd = 2)
lines(F1_noH$Year, F1_noH$Crate, col = "green", lwd = 2)
lines(F4_default_SI$Year, F4_default_SI$Crate, col = "red4", lwd = 2)
#lines(US_Ho1$Year, US_Ho1$Crate, col = "purple", lwd = 2)
#lines(US_Ha1$Year, US_Ha1$Crate, col = "orange", lwd = 2)
#lines(US_Cwt$Year, US_Cwt$Crate, col = "cyan", lwd = 2)

ggplot() +
  geom_point(data = F3_oldData, aes(x = Year, y = Crate), color = "blue", size = 2) +
  geom_line(data = F3_oldData, aes(x = Year, y = Crate), color = "blue", size = 2) +
  geom_point(data = F2, aes(x = Year, y = Crate), color = "red", size = 2, shape = 17) +
  geom_line(data = F2, aes(x = Year, y = Crate), color = "red", size = 2) +
  labs(
    title = "Change in carbon uptake MMSF",
    x = "Year",
    y = "Change in carbon uptake (MT C/Ha/Year)"
  ) +
  scale_x_continuous(limits = c(min(F3_oldData$Year, F2$Year), max(F3_oldData$Year, F2$Year))) +
  scale_y_continuous(limits = c(0, 4))

ggplot() +
  geom_point(data = F3_oldData, aes(x = Year, y = Crate), color = "firebrick1", size = 2) +
  geom_line(data = F3_oldData, aes(x = Year, y = Crate), color = "firebrick1", size = 2, linetype = "solid") +
  geom_point(data = F2, aes(x = Year, y = Crate), color = "royalblue1", size = 2, shape = 17) +
  geom_line(data = F2, aes(x = Year, y = Crate), color = "royalblue1", size = 2, linetype = "dashed") +
  labs(
    title = "Change in carbon uptake MMSF",
    x = "Year",
    y = "Change in carbon uptake (MT C/Ha/Year)"
  ) +
  scale_x_continuous(limits = c(min(F3_oldData$Year, F2$Year), max(F3_oldData$Year, F2$Year)) +
                       scale_y_continuous(limits = c(0, 4)))


###labels
ggplot() +
  geom_point(data = F3_oldData, aes(x = Year, y = Crate, color = "Local Site Index"), size = 1) +
  geom_line(data = F3_oldData, aes(x = Year, y = Crate, color = "Local Site Index"), size = 1, linetype = "solid") +
  geom_point(data = F2, aes(x = Year, y = Crate, color = "Default Site Index"), size = 1) +
  geom_line(data = F2, aes(x = Year, y = Crate, color = "Default Site Index"), size = 1, linetype = "dashed") +
  labs(
    title = "Change in biomass UMB",
    x = "Year",
    y = "Change in biomass (MT C/Ha/Year)"
  ) +
  scale_x_continuous(limits = c(min(F3_oldData$Year, F2$Year), max(F3_oldData$Year, F2$Year)) +
                       scale_y_continuous(limits = c(0, 4)) +
                       scale_color_manual(
                         values = c("Local Site Index" = "firebrick1", "Default Site Index" = "black"),
                         name = "FVS"))


# Create a ggplot object

                     #This code should now correctly use "firebrick1" for "Local Site Index" and "royalblue1" for "Default Site Index" as the colors for your plot.
                     
                     
                     
                     
                     
                     







# Add legend
legend("topright", legend = c("MMSF_2022 data", "MMSF 2023", "MMSF 2023_no height", "MMSF 2023_Default SI"),
       col = c("blue", "red", "green", "red4"), lty = 2, cex = 0.5)
