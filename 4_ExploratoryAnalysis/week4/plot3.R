nei.baltimore <- NEI %>% filter(fips == "24510") %>% group_by(type, year) %>% summarize(Annual.Total = sum(Emissions));
nei.baltimore$type <- factor(nei.baltimore$type, levels = c("ON-ROAD", "NON-ROAD", "POINT", "NONPOINT")) # Re-order factor levels so they plot in the order we wish
ggplot(nei.baltimore, aes(x = factor(year), y = Annual.Total, fill = type)) + 
  geom_bar(stat = "identity") + 
  facet_grid(. ~ type) + 
  xlab("Year") + 
  ylab(expression("Total Tons of PM"[2.5]*" Emissions")) + 
  ggtitle(expression("Total Tons of PM"[2.5]*" Emissions in Baltimore by Source Type")) +
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_y_continuous(labels = comma) +
  guides(fill = FALSE)