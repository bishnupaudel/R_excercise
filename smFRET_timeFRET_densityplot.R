library(ggplot2)
library(plotly)
library(reshape2)
library(ggExtra)
library(egg)

setSessionTimeLimit(cpu = Inf, elapsed = Inf)
data_1 = read.table("F:/PPR/processed/PPR only/path/mu 1.0 beta 1.0/heatmap.dat", header = F)
T_1 = A$V1/5
FRET = A$V4


p1 <- ggplot(data_1, mapping = aes(x = T_1, y = FRET)) + geom_hex(bins = 150, size = 0.5, alpha = 1) +
  geom_density2d(colour = "#FFFFFF", size = 0.5, bins = 10, alpha = 1) +
  scale_fill_continuous(type = "viridis") +
  xlim(0,100) + ylim (0,1.1) + xlab("Time (s)") + ylab("FRET")
p1


p2 <- p1 + theme(legend.position = "none")


p3 <- ggplot(data_1, aes(x = FRET))  + geom_histogram(aes(x=FRET, y = ..density..), binwidth=0.05,
                                                      fill='#4B0082', color = 'black') +
   xlab("") + ylab("") + xlim(0,1.1) +
  theme(axis.ticks.y=element_blank(), axis.text.y=element_blank())
p3

p4 <- p3 + coord_flip()
p4

# ggarrange(p2, p4, nrow=2)
p5 <- grid.arrange(p2, p4, ncol=2, nrow=2, widths=c(0.2, 0.05), heights=c(0.2, 0.05))
p5
ggsave("p5.emf")