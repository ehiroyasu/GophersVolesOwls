#Plotting gopher plots
setwd("../GophersVolesOwls")
library(devtools)
library(roxygen2)
load_all("owls")
library(owls)

#To allow for greek letters:
Sys.setlocale('LC_CTYPE', 'greek')

###Gopher Plots:

gopher_df <- read.csv("./data/Processed/GopherTable051518.csv", header = TRUE) %>%
  rename(K_prey = K)

### Let's chunk out into nine K/alphaP plots, each with a unique title, and each with nine facets based on N0 and r.
### Each plot will go into a list; then we can use cowplot to grid them together.Since we need a title for the 
### nine K/alphaP plots, let's create that in the dataframe directly; this also provides a nice handle for filtering
### the dataframe for the various plots.

gopher_df <- gopher_df %>%
  mutate(lbl = sprintf('K = %s, α = %s', K_prey, alphaP))

### let's create a dummy plot we can grab the legend from
df <- gopher_df %>%
  filter(lbl == first(lbl))
tmp <- k_alpha_plot(df, ymaxK=200, clean = FALSE)
lgd <- get_legend(tmp)
k_alpha_vec <- gopher_df$lbl %>% unique()

### use a loop or lapply to create each plot.
plot_list <- lapply(k_alpha_vec, FUN = function(x) { ### x <- k_alpha_vec[1]
    df <- gopher_df %>%
      filter(lbl == x)
    k_alpha_plot(df, ymaxK=200)
  }) %>%
  setNames(k_alpha_vec)

#For nine by nine plot, containing all 81 plots
#nine_by_nine_plot <- plot_grid(plotlist = plot_list)

## to do 9 single nine-by-nine plots 
gopher_plots<-NULL
for(i in 1:length(plot_list)){
  gopher_plots[[i]]<-ggdraw()+
    draw_plot(plot_list[[i]],x = 0.05, y = 0.05, width = .75, height = .95)+
    draw_plot_label(label = 'Time (Seasons)', x = .5, y = 0.01, vjust = 0, hjust = .5) +
    draw_plot_label(label = 'Prey Density (individuals/ha)', x = 0.01, y = .5, angle = 90,
                    hjust = .5, vjust = 1) +
    draw_grob(lgd, x = .80, y = .25, width = .18, height = .5)
}

for (i in 1:length(k_alpha_vec)){
  ggsave(paste(str_replace_all(k_alpha_vec, " ", "")[i],".tiff"), gopher_plots[[i]], path="./Output/Figures/Gopher/BW/082318Figs")
}

################################################################################################################
##Vole Plots
vole_df<-read.csv("./data/Processed/VoleTable051518.csv", header=TRUE) %>% 
  rename(K_prey=K)

vole_df<-vole_df %>%
  mutate(lbl=sprintf('K = %s, α = %s', K_prey, alphaP))

### let's create a dummy plot we can grab the legend from
df <- vole_df %>%
  filter(lbl == first(lbl))
tmp <- k_alpha_plot(df, ymaxK=1000, clean = FALSE)
lgd <- get_legend(tmp)
k_alpha_vec <- vole_df$lbl %>% unique()

### use a loop or lapply to create each plot.
plot_list <- lapply(k_alpha_vec, FUN = function(x) { ### x <- k_alpha_vec[1]
  df <- vole_df %>%
    filter(lbl == x)
  k_alpha_plot(df, ymaxK=1000)
}) %>%
  setNames(k_alpha_vec)

#For nine by nine plot, containing all 81 plots
#nine_by_nine_plot <- plot_grid(plotlist = plot_list)

## to do 9 single nine-by-nine plots 
vole_plots<-NULL
for(i in 1:length(plot_list)){
  vole_plots[[i]]<-ggdraw()+
    draw_plot(plot_list[[i]],x = 0.05, y = 0.05, width = .75, height = .95)+
    draw_plot_label(label = 'Time (Seasons)', x = .5, y = 0.01, vjust = 0, hjust = .5) +
    draw_plot_label(label = 'Prey Density (individuals/ha)', x = 0.01, y = .5, angle = 90,
                    hjust = .5, vjust = 1) +
    draw_grob(lgd, x = .80, y = .25, width = .18, height = .5)
}

for (i in 1:length(k_alpha_vec)){
  ggsave(paste(str_replace_all(k_alpha_vec, " ", "")[i],".tiff"), vole_plots[[i]], path="./Output/Figures/Vole/BW/082318Figs")
}

