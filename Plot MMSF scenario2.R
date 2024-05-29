# Set up the plotting area
US_Ha1$Year <- as.numeric(US_Ha1$Year)
F3_oldData$Year <- as.numeric(F3_oldData$Year)

# Set up the plot with appropriate limits and labels
plot(F3_oldData$Year, F3_oldData$Crate, type = "n", xlim = c(min(F3_oldData$Year), max(F3_oldData$Year)), ylim = c(0, 4),
     main = "Change in carbon uptake MMSF", xlab = "Year", ylab = "Change in carbon uptake (MT C/Ha/Year)")
# Plot each dataset
lines(F3_oldData$Year, F3_oldData$Crate, col = "blue", lwd = 2)
lines(F2$Year, F2$Crate, col = "red", lwd = 2)
lines(F1_noH$Year, F1_noH$Crate, col = "green", lwd = 2)
lines(F4_default_SI$Year, F4_default_SI$Crate, col = "red4", lwd = 2)
lines(US_MMSF_2023_2$Year,US_MMSF_2023_2$Crate, col =  "plum4", lwd = 2 )
#lines(US_Ho1$Year, US_Ho1$Crate, col = "purple", lwd = 2)
#lines(US_Ha1$Year, US_Ha1$Crate, col = "orange", lwd = 2)
#lines(US_Cwt$Year, US_Cwt$Crate, col = "cyan", lwd = 2)

# Add legend
legend("topright", legend = c("MMSF_2022 data", "MMSF 2023", "MMSF 2023_no height", "MMSF 2023_Default SI", "MMSF 2023_2"),
       col = c("blue", "red", "green", "red4", "plum4"), lty = 2, cex = 0.5)
