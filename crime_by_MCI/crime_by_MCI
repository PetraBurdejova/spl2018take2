source(Merging.R)

# Group crimes by MCI
mci.group <- group_by(crime.dt, MCI)
crime.by.mci <- dplyr::summarise(mci.group, n = n()) # count of events by MCI
crime.by.mci <- crime.by.mci[order(crime.by.mci$n, decreasing = TRUE), ] # order crime by type from most to least 

# plot crimes by type, order by most common to least common
 plot(ggplot(aes(x = reorder(MCI, n), y = n), data = crime.by.mci) + # order MCI's by number of occurrences, use crime by MCI data
  geom_bar(stat = 'identity', width = 0.5) + # bar graph
  geom_text(aes(label = n), stat = 'identity', data = crime.by.mci, hjust = -0.1, size = 3.5) +
  coord_flip() + # flip x and y coordinates
  xlab('Major Crime Indicators') + # x-axis label
  ylab('Number of Occurrences') + # y-axis label 
  ggtitle('Major Crime Indicators Toronto 2016') + # title
  theme_bw() + 
  theme(plot.title = element_text(size = 16),
        axis.title = element_text(size = 12, face = "bold")))

