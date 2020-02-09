require(ggplot2)
ggplot() +
  
  geom_point(
    data = 
      data.frame(
        x = c(0,0,0),
        y = c(1,2,3),
        class = c("Alive", "Dead", "Dead"),
        Species = c("LACI", "LANO", "LABO")
      ),
    aes(x = x , y = y, shape = class, color = Species),
    size = 11, alpha = 0.85, stroke = 1.6
    ) +
  scale_shape_manual(name = "Observation type", values = c(21,23)) +
  scale_color_manual(values = rev(viridis::plasma(3)), labels = c("LACI", "LABO", "LANO")) +
  
  geom_rect(
    data = 
      data.frame(x1=c(1,3,1,5,4), x2=c(2,4,3,6,6), y1=c(1,1,4,1,3), y2=c(2,2,5,3,5), t=rep("Karst areas", 5), r=c(1,2,3,4,5)), 
    aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=t), 
    color=NA
    ) +
  scale_fill_manual(name = NULL, values = "grey90") +
  
  guides(
    color = guide_legend(order = 1, override.aes = list(shape = 15)),
    shape = guide_legend(order = 2),
    fill = guide_legend(order = 3)
  ) +
  
  theme_minimal() +
  theme(
    text = element_text(size = 10)
  )

ggsave(filename = file.path(wd$figs, "legend_example.tif"), device = "tiff", width = 2, height = 4, units = "in")



# Approach 2 --------------------------------------------------------------

library(cowplot)
library(dplyr)

theme_set(theme_minimal())

allScales <-
  c("4" = "red"
    , "6" = "orange"
    , "8" = "green"
    , "0" = "blue"
    , "1" = "black"
    , "High MPG" = "cyan"
    , "Other Region" = "yellow")

mainPlot <-
  ggplot(mtcars, aes(x = disp
                     , y = mpg)) +
  ##region for high mpg 
  geom_rect(aes(fill = "High MPG")
            , xmin = min(mtcars$disp)-5
            , ymax = max(mtcars$mpg) + 2
            , xmax = mean(range(mtcars$disp))
            , ymin = 25
            , alpha = 0.02) + 
  ## test diff region
  geom_rect(aes(fill = "Other Region")
            , xmin = 300
            , xmax = 400
            , ymax = 30
            , ymin = 25
            , alpha = 0.02) + 
  geom_point(aes(fill = factor(vs)),shape = 23, size = 8, alpha = 0.4) +
  geom_point (aes(fill = factor(cyl)),shape = 21, size = 2) +
  scale_fill_manual(values = allScales)

vsLeg <-
  (ggplot(mtcars, aes(x = disp
                      , y = mpg)) +
     geom_point(aes(fill = factor(vs)), shape = 23, size = 8, alpha = 0.4) +
     scale_fill_manual(values = allScales
                       , name = "VS")
  ) %>%
  ggplotGrob %>%
  {.$grobs[[which(sapply(.$grobs, function(x) {x$name}) == "guide-box")]]}



cylLeg <-
  (ggplot(mtcars, aes(x = disp
                      , y = mpg)) +
     geom_point (aes(fill = factor(cyl)),shape = 21, size = 2) +
     scale_fill_manual(values = allScales
                       , name = "Cylinders")
  ) %>%
  ggplotGrob %>%
  {.$grobs[[which(sapply(.$grobs, function(x) {x$name}) == "guide-box")]]}


regionLeg <-
  (ggplot(mtcars, aes(x = disp
                      , y = mpg)) +
     geom_rect(aes(fill = "High MPG")
               , xmin = min(mtcars$disp)-5
               , ymax = max(mtcars$mpg) + 2
               , xmax = mean(range(mtcars$disp))
               , ymin = 25
               , alpha = 0.02) + 
     ## test diff region
     geom_rect(aes(fill = "Other Region")
               , xmin = 300
               , xmax = 400
               , ymax = 30
               , ymin = 25
               , alpha = 0.02) + 
     scale_fill_manual(values = allScales
                       , name = "Region"
                       , guide = guide_legend(override.aes = list(alpha = 0.4)))
  ) %>%
  ggplotGrob %>%
  {.$grobs[[which(sapply(.$grobs, function(x) {x$name}) == "guide-box")]]}


legendColumn <-
  plot_grid(
    # To make space at the top
    vsLeg + theme(legend.position = "none")
    # Plot the legends
    , vsLeg, regionLeg, cylLeg
    # To make space at the bottom
    , vsLeg + theme(legend.position = "none")
    , ncol = 1
    , align = "v")

plot_grid(mainPlot +
            theme(legend.position = "none")
          , legendColumn
          , rel_widths = c(1,.25))
