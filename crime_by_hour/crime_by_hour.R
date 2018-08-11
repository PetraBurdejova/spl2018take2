source("Merging.R")

# Group crimes by time of day
hour.group <- group_by(crime.dt, occurrencehour)
crime.hour <- dplyr::summarise(hour.group, n = n()) # count of crimes by hour

# plot crime by time of day
plot(ggplot(aes(x=occurrencehour, y = n), data = crime.hour) + geom_line(size = 2.5, alpha = 0.7, color = "mediumseagreen", group=1) + 
  geom_point(size = 0.5) + 
  ggtitle('Total Crimes by Hour of Day in Toronto 2016') +
  ylab('Number of Occurrences') +
  xlab('Hour(24-hour clock)') +
  theme_bw() +
  theme(plot.title = element_text(size = 16),
        axis.title = element_text(size = 12, face = "bold")))


# Crimes by MCI by hour
crime.type.by.hour <- group_by(crime.dt, occurrencehour, MCI) # group data by hour and MCI
hour.crime <- dplyr::summarise(crime.type.by.hour, n = n()) # count of crime types by hour

plot(ggplot(aes(x = occurrencehour, y = n, color = MCI, linetype = MCI), data = hour.crime) + # plot lines by MCI 
  geom_line(size = 1.5) + 
  ggtitle('Crime Types by Hour of Day in Toronto 2016') +
  ylab('Number of Occurrences') +
  xlab('Hour(24-hour clock)') +
  theme_bw() +
  theme(plot.title = element_text(size = 16),
        axis.title = element_text(size = 12, face = "bold")))
