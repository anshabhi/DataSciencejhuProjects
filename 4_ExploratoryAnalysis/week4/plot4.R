scc.coal <- SCC[grep("Fuel Comb.*Coal", SCC$EI.Sector),  ];
scc.coal.list <- unique(scc.coal$SCC);
nei.coal <- subset(NEI, SCC %in% scc.coal.list);
nei.coal <- nei.coal %>% group_by(type, year) %>% summarize(Annual.Total = sum(Emissions))
nei.coal.total <- nei.coal %>% group_by(year) %>% summarize(Annual.Total = sum(Annual.Total)) %>% mutate(type = "TOTAL");
nei.coal <- nei.coal %>% select(Annual.Total, type, year);
nei.coal <- bind_rows(nei.coal, nei.coal.total);
nei.coal$type <- factor(nei.coal$type, levels = c("TOTAL", "ON-ROAD", "NON-ROAD", "POINT", "NONPOINT")); # Re-order factor levels to they plot in the order we wish
ggplot(nei.coal, aes(x = factor(year), y = Annual.Total, fill = type)) +
  geom_bar(stat = "identity") +
  facet_grid(. ~ type) +
  xlab("Year") +
  ylab(expression("Total Tons of PM"[2.5]*" Emissions")) + 
  ggtitle(expression(atop("Total Tons of PM"[2.5]*" Emissions in the United States", paste("from Coal Combustion-Related Sources")))) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_y_continuous(labels = comma) +
  scale_fill_brewer(palette = "Dark2") +
  guides(fill = FALSE)