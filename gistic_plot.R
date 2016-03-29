library(ggplot2)
library(grid)
old <- theme_set(theme_minimal(20))

gistic_plot <- function(scores.gistic = 'scores.gistic'){
  gistic <- fread(scores.gistic)
  ggplot(gistic, aes(x=Start, xend=End, y=`-log10(q-value)`, yend=`-log10(q-value)`, col=Type)) +
    geom_line() +
    facet_grid(Type~Chromosome,
               scales = "free_x",
               space = "free_x") +
    scale_y_continuous(expand = c(0,0)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          panel.background = element_rect(colour="gray"),
          panel.margin = unit(0, "cm"),
          legend.position = "none") +
    scale_color_manual(values=c("red", "blue"))

  ggsave(file=paste0(scores.gistic, '.pdf'), width = 15, height = 10)
}
