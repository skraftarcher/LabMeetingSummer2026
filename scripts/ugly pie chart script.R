#Mary Earl Varat script for ugly figure 

library(ggplot2)

library(ggplot2)

ggplot(b2, aes(x = "", y = abundance, fill = taxaID)) +
  geom_bar(stat = "identity", width = 1, color = "black", linewidth = 0.3) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Abundance", fill = "Taxa ID")
  