#'k_alpha_plot
#'
#'Uses ggplot2 package to plot the output data from predprey_sim in black and white. 
#'
#'@param df is the dataframe to feed into ggplot2, it is the subsetted dataframe from the df_sim function in the 'owls' package.
#'@param ymaxK is the max limit for the y-axis, it is different depending on whether we're plotting for gophers or voles
#'
#'@author Elizabeth Hiroyasu & Casey O'Hara
#'
#'


### let's write a ggplot function to create the facet grid for a single
### K/alpha combo.  Leave out X and Y axes, otherwise ends up duplicating
### when all combined into a big plot. Drop legend as default...


k_alpha_plot <- function(df, ymaxK, clean = TRUE) {
  
  plot_lbl <- df$lbl %>% unique()
  if(length(plot_lbl) != 1) stop('dataframe needs to have exactly one label')
  
  x <- ggplot(data = df, aes(x = time, y = N, group = as.factor(P), linetype = as.factor(P))) +
    geom_line(size = 1) +
    scale_linetype_manual(values = c(1,2,3), name = "Predator Density") +
    labs(title = plot_lbl) +
    scale_y_log10(labels=comma, limits=c(1, ymaxK))+
    geom_hline(aes(yintercept = K_prey), linetype = "dashed") +
    facet_grid(N0 ~ r, labeller = label_bquote(rows=N[0]:~.(N0), cols=r:~.(r))) +
    theme(plot.title=element_text(size=14, family="sans"), 
          panel.background = element_rect(colour = "black", fill=NA, size=0.5, linetype=1),
          axis.title=element_blank())
  
  #theme(plot.title=element_text(size=12, family = "sans"), 
  #      axis.title = element_blank(),
  #      strip.background = element_blank())
  
  if(clean == TRUE) {
    x <- x +
      theme(legend.position = 'none')
  }
  
  return(x)
}