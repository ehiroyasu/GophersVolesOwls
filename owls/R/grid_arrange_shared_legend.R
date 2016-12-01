#'Grid arrange with a shared legend
#'
#'Uses the gridExtra and grid package to bring plots together with a single legend.
#'
#'Available at https://github.com/tidyverse/ggplot2
#'
#'@author Hadley Wickham
#'
#'


grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight), 
    top=textGrob("Prey Density over Time", gp=gpar(fontface="bold", fontsize=6, cex=3)))
}