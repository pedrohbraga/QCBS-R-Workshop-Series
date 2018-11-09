# QCBS R Workshop Series ##

## ggplot2 ##

## Author: Quebec Center for Biodiversity Science
## Materials Generated & Amalgamated by: 
## Xavier Giroux-Bougard, Monica Granados,
## Maxwell Farrell, Etienne Low-Decarie,
## Rémi Maglione, Marc-Olivier Beausoleil
## Last updated: November 2018
## Built under R version 3.3.3 

#------------------------------------------------------------#
# Package Needed
#------------------------------------------------------------#

if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggplot2)) library(ggplot2)

if(!require(knitr)) install.packages("knitr")
if(!require(knitr)) library(knitr)

if(!require(tidyr)) install.packages("tidyr")
if(!require(tidyr)) library(tidyr)

if(!require(dplyr)) install.packages("dplyr")
if(!require(dplyr)) library(dplyr)

if(!require(magrittr)) install.packages("magrittr")
if(!require(magrittr)) library(magrittr)

if(!require(grid)) install.packages("grid")
if(!require(grid)) library(grid)

if(!require(gridExtra)) install.packages("gridExtra")
if(!require(gridExtra)) require(gridExtra)

if(!require(RColorBrewer)) install.packages("RColorBrewer")
if(!require(RColorBrewer)) library(RColorBrewer)

if(!require(kableExtra)) install.packages("kableExtra")
if(!require(kableExtra)) library(kableExtra)

if(!require(gridBase)) install.packages("gridBase")
if(!require(gridBase))library(gridBase)

if(!require(vegan)) install.packages("vegan")
if(!require(vegan)) library(vegan)

if(!require(vioplot)) install.packages("vioplot")
if(!require(vioplot)) library(vioplot)

if(!require(ggpubr)) install.packages("ggpubr")
if(!require(ggpubr)) library(ggpubr)

if(!require(ggsignif))install.packages("ggsignif")
if(!require(ggpubr)) library(ggpubr)

if(!require(ggvegan)) install.packages("ggvegan")
if(!require(ggpubr)) library(ggpubr)

if(!require(ggdendro)) install.packages("ggdendro")
if(!require(ggdendro)) library(ggdendro)

if(!require(rworldmap)) install.packages("rworldmap")
if(!require(rworldmap)) library(rworldmap)

if(!require(maps)) install.packages("maps")
if(!require(maps)) library(maps)

if(!require(mapproj)) install.packages("mapproj")
if(!require(mapproj)) library(mapproj)

if(!require(reshape2)) install.packages("reshape2")
if(!require(reshape2)) library(reshape2)

if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

if (!require(plotly)) install.packages("plotly")
library(plotly)

#source(file="./multiplot.R")

#------------------------------------------------------------#
# Introduction
#------------------------------------------------------------#
## Introduction : Why use R for plotting?
### Have you created plots?
### Have you plotted in R?

#------------------------------------------------------------#
# ggplot2
#------------------------------------------------------------#

#------------------------------------------------------------#  
# ** Required packages ** #
#------------------------------------------------------------#
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggplot2)) library(ggplot2)

#------------------------------------------------------------#
# Grammar of Graphics (GG)
#------------------------------------------------------------#
ggplot(data = iris,             # Data
       aes(x = Sepal.Length,    # Your X-value
           y = Sepal.Width,     # Your Y-value
           col = Species)) +    # Aesthtics
  geom_point() +                # Geometry
  geom_smooth(method = "lm") +  # Linear regression
  ggtitle("My fabulous graph")+  # Title
  theme(plot.title=
          element_text(color="red",
                       size=14,
                       face="bold.italic")) # Theme


#------------------------------------------------------------#
# Importance in data structure
#------------------------------------------------------------#
head(iris[,c("Sepal.Length","Sepal.Width","Species")], n = 5)


#------------------------------------------------------------#
# Exploring your data (optional) 
#------------------------------------------------------------#
panel.hist <- function(x, ...)
  {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="blue4", ...)
}

panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
  {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, method = "pearson")) # can be "spearman"
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.7/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * r)
}
betterPairs <- function(YourData, col = c("black"))
  {
  return(pairs(YourData, 
               lower.panel=function(...) {
                 par(new=TRUE);
                 panel.smooth(col=col,
                              col.smooth = "gold",...)},
               diag.panel=panel.hist, 
               upper.panel=panel.cor))
}

betterPairs(iris) # Print your data exploration

#------------------------------------------------------------#
# A simple Example with iris database
#------------------------------------------------------------#
## **Inheritance from ggplot**
ggplot(data = iris,             # Data
       aes(x = Sepal.Length,    # Your X-value
           y = Sepal.Width)) +  # Your Y-value
  geom_point()                  # Geometry      
      
## **Storing ggplot in object**
p <- ggplot(data = iris,
            aes(x = Sepal.Length,
                y = Sepal.Width))
q <- p + geom_point()
q   # Print your final plot
 
## **No inheritance from ggplot**
ggplot() + # base ggplot layer without data 
  geom_point(data = iris,
             aes(x = Sepal.Length,
                 y = Sepal.Width))

## **Adding layer from ggplot object**

s <- ggplot() # base object ggplot layer without data 
s <- s +
  geom_point(data = iris,
             aes(x = Sepal.Length,
                 y = Sepal.Width))
s
      
#------------------------------------------------------------#
# GGplot dynamics: base layer
#------------------------------------------------------------#
ggplot() + 
  scale_x_continuous() +
  scale_y_continuous() 


#------------------------------------------------------------#
# GGplot dynamics: Data layer
#------------------------------------------------------------#
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  xlab("x = Sepal Lenght") +
  ylab("y = Sepal Width")


#------------------------------------------------------------#
# GGplot dynamics: Gemoetric Layer
#------------------------------------------------------------#
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  xlab("x = Sepal Lenght") + ylab("y = Sepal Width") + 
  geom_point()


#------------------------------------------------------------#
# Challenge #1 (5-10min)
#------------------------------------------------------------#
# * Draw your 1rst (gg)plot:
## Data : iris
## geom : geom_point()
## x value : Petal length
## y value : Petal width
  
### Solution at the end of this file


#------------------------------------------------------------#
# Saving plots in RStudio
#------------------------------------------------------------#
# Saving plots with ggsave() function
##ggsave() will write directly to your working directory all in one line of code and you can specify the name of the file and the dimensions of the plot:
my1rstPlot <- ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width)) + geom_point()
ggsave("my1rstPlot.pdf",
       my1rstPlot,
       height = 8.5,
       width = 11,
       units = "in")

#------------------------------------------------------------#
#Other methods to save image
#------------------------------------------------------------#
?pdf
?jpeg

pdf("./graph_du_jour.pdf")
print(my1rstPlot)
graphics.off()

#------------------------------------------------------------#
# Aesthetic
#------------------------------------------------------------#
# Example of aesthetic: color
## Without Color
ggplot(data = iris, aes(x = Sepal.Length, 
                        y = Sepal.Width)) +
  geom_point() 

## With Color
ggplot(data = iris, aes(x = Sepal.Length, 
                              y = Sepal.Width, 
                              color = Species)) +
        geom_point() + theme(legend.position = 'bottom')


#------------------------------------------------------------#
# Challenge #2
#------------------------------------------------------------#
## Produce a colourful plot from built in data
### Data : iris, CO2 , msleep
### x value : Petal.Length, conc , log10(bodywt)
### y value : Petal.Width, uptake , awake
### Aesthetic : Species, Treatment & Type , vore & conservation


#------------------------------------------------------------#
# Facet 
#------------------------------------------------------------#

## Facet : Iris
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, 
                        color = Species)) +
  geom_point() + 
  facet_grid(~Species, scales = "free") 

## Facet : CO2
CO2.plot <- ggplot(data = CO2, aes(x = conc, y = uptake, colour = Treatment)) +
  geom_point() +
  xlab("CO2 Concentration (mL/L)") +
  ylab("CO2 Uptake (umol/m^2 sec)") 
CO2.plot.facet <- CO2.plot + facet_grid(~ Type) + ggtitle("Facet")

multiplot(CO2.plot.nofacet, CO2.plot.facet)


#------------------------------------------------------------#
# Fine Tunning - Colour
#------------------------------------------------------------#

## ** Manual **

iris.plot <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point()
iris.plot + scale_colour_manual(values = c("setosa" = "red", 
                                           "versicolor" = "darkgreen",
                                           "virginica"="blue"))

## **Classic**
iris.plot <- ggplot(data = iris, 
                    aes(x = Sepal.Length, 
                        y = Sepal.Width, 
                        color = Species)) +
  geom_point()
iris.plot


#------------------------------------------------------------# 
  # Fine tuning - Gradient colours for quantitative variable
#------------------------------------------------------------#
#  *quantitavie variable : Petal.Length*
# Calssic 
iris.plot.petal <- ggplot(data = iris, 
                          aes(x = Sepal.Length, 
                              y = Sepal.Width, 
                              color = Petal.Length)) +
  geom_point()
iris.plot.petal

#Manual 
  iris.plot.petal <- ggplot(data = iris, 
                            aes(x = Sepal.Length, 
                                y = Sepal.Width, 
                                color = Petal.Length)) +
    geom_point()
  iris.plot.petal + scale_color_gradient(low="blue", high="red")

  
#------------------------------------------------------------# 
# Fine tuning - colours theme
#------------------------------------------------------------# 
# Coulours theme : RColorBrewer
if (!require(RColorBrewer)) installed.packages("RColorBrewer")
require(RColorBrewer)

#  **Classic**
iris.plot

#  **Dark2**
  iris.plot + scale_color_brewer(palette = "Dark2")

  
#------------------------------------------------------------# 
# Blind color friendly : presentation purpose
#------------------------------------------------------------# 
#  **Classic**
  iris.plot
    
#  **Blind color friendly**
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  iris.plot + scale_color_manual(values=cbbPalette)

  
#------------------------------------------------------------#
# black & white : grey scale for publication purpose
#------------------------------------------------------------#
#  **Defaut**
  iris.plot
  
# **grey scale
  iris.plot + scale_color_grey()

  
#------------------------------------------------------------#
# Theme classic for publication purpose
#------------------------------------------------------------#
# **Defaut**
iris.plot+ scale_color_grey()

#  **Classic**
iris.plot + scale_color_grey() + theme_classic()

#------------------------------------------------------------#
# Axis & Title tunning
#------------------------------------------------------------#
iris.plot + 
  ggtitle("Relation between Sepal Lenght & Width") +
  xlab("Sepal length (cm)") + 
  ylab("Sepal Width (cm)") +
  theme(axis.title.x = element_text(size = 16, colour = cbbPalette[6]),
        axis.title.y = element_text(size = 16, colour = cbbPalette[6]),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, face="bold"),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size=12))


#------------------------------------------------------------#    
#                      Challenge #3
#------------------------------------------------------------#
#* Voici un graphique issu du set de données « tips » inclu avec la librarie "reshape2"
library(reshape2)


#------------------------------------------------------------#  
# Theme assist : The perfect RStudio Addin for GGplot2
#------------------------------------------------------------#  
if (!requireNamespace("devtools", quietly = TRUE))
install.packages("devtools")
if (!require(ggThemeAssist)) devtools::install_github("calligross/ggthemeassist")
# restart rstudio after Addin install and load the packages
require(ggThemeAssist)

#HOW IT WORK ?
#1rst :select/hilight your graph, either in your consol or your scipt screen

#2nd: select « GGplot Theme assistant » from the RStudio Addins manager
#(Addins should be located below your main menu bar, at the right of your tool bar)

#3rd: Strat editing every ggplot theeme parameter. Enjoy !

#------------------------------------------------------------#
#Geom : GGplot
#------------------------------------------------------------#

#------------------------------------------------------------#
##Blank plot
#------------------------------------------------------------#
#*GGplot2*
gblank <- ggplot(mtcars, aes(wt, mpg))
gblank

#*Base Plot* (optional)
plot(mpg~wt, data = mtcars, type = "n")

#------------------------------------------------------------#
#Histogram
#------------------------------------------------------------#
#*GGplot2*
ggplot(diamonds, aes(carat)) + 
  geom_histogram(binwidth = .5) + # binwidth : chose your histogram bar width
  labs(title = "Histogram") # Add a title

# *Base Plot* (optional)
hist(diamonds$carat)


#------------------------------------------------------------#
#Barplot
#------------------------------------------------------------#
#*GGplot2*
ggplot(mpg, aes(class)) + # Dataset mpg
  geom_bar() + 
  labs(title = "Barplot")


#*Base Plot* (optional)
barplot(table(mpg$class))

#------------------------------------------------------------#
# Significant values
#------------------------------------------------------------#
if(!require(tidyr)) install.packages("tidyr")
if(!require(tidyr)) library(tidyr)

if(!require(dplyr)) install.packages("dplyr")
if(!require(dplyr)) library(dplyr)

if(!require(ggsignif)) install.packages("ggsignif")
if(!require(ggsignif)) library(ggsignif)

#*GGplot2*
dat <- data.frame(Group = c("S1", "S1", "S2", "S2"),
Sub   = c("A", "B", "A", "B"),
Value = c(3,5,7,8),
low = c(2.5,4.5,6,7.5),
high = c(3.5,5.5,7,8.5))  


gbar2 <- ggplot(data = dat, aes(x = Group, y = Value,fill = Sub)) +
  geom_bar(#aes(fill = Sub), 
    stat="identity", 
    position="dodge", 
    width=.5) +
  geom_errorbar(aes(ymin=low, 
                    ymax=high),
                width = 0.1,
                position =  position_dodge(.5), 
                colour="black") +
  labs(title = "Barplot") +
  scale_fill_manual(values = c("grey80", 
                               "grey20")) + 
  geom_signif(stat="identity",
              data=data.frame(x=c(0.875, 1.875),
                              xend=c(1.125, 2.125),
                              y=c(5.8, 8.8),
                              Sub=c("A","B"),
                              annotation=c("**", "NS")),
              aes(x=x,xend=xend, y=y, yend=y,
                  annotation=annotation)) +
  geom_signif(comparisons=list(c("S1", "S2")),
              annotations="***",
              y_position = 9.3,
              tip_length = 0,
              vjust=0.4)

gbar2


#*Base Plot* (optional)
df=data.frame(S1 = c(3,5), S2=c(7,8))

my.bp = barplot(as.matrix(df),
               beside = TRUE,
               xlab = "Group",
               ylab = "Value",
               main = "Barplot",
               ylim = c(0,12),
               legend = rownames(df),
               args.legend = list(x = "topright", 
                                  bty = "n", 
                                  inset=c(-0.07, 0)))

# Manueal Function
sign.bar = function(pos,select.pair, y, offset = 0.2, label, mid = FALSE) {
  # create the y coordinate of the line
  y <- y
  # set an offset for tick lengths
  offset <- offset
  # draw first horizontal line
  if (!mid) {
    lines(pos[select.pair],c(y, y))
    # draw ticks
    lines(pos[c(select.pair[1],select.pair[1])],c(y, y-offset))
    lines(pos[c(select.pair[2],select.pair[2])],c(y, y-offset))
    } else {
      lines(x = c(c(pos[select.pair[1]-1]+
                      pos[select.pair[1]])/2,
                  c(pos[select.pair[2]]+
                      pos[select.pair[2]+1])/2),
            c(y, y))
      # draw ticks
      lines(c(c(pos[select.pair[1]-1]+
                  pos[select.pair[1]])/2,
              c(pos[select.pair[1]-1]+
                  pos[select.pair[1]])/2),
            c(y, y-offset))
      lines(c(c(pos[select.pair[2]]+
                  pos[select.pair[2]+1])/2,
              c(pos[select.pair[2]]+
                  pos[select.pair[2]+1])/2),
            c(y, y-offset))
      }
  # draw asterics
  text(pos[select.pair[1]]+((pos[select.pair[2]]-pos[select.pair[1]])/2),y+offset*2,
       labels = label) 
}

# Add your bar to your plot
sign.bar(my.bp, select.pair = c(1,2),y =6,label = "**")
sign.bar(my.bp, select.pair = c(3,4),y =8.8,label = "NS")
sign.bar(my.bp, select.pair = c(2,3),y =10,label = "***",mid = TRUE)


#------------------------------------------------------------#
#Plot: linear reagression
#------------------------------------------------------------#
#*GGplot2*
ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  geom_smooth(method = lm)+ 
  labs(title = "Plot")

#*Base Plot* (optional)
lm.out  <- lm(hwy~displ, data = mpg) #linear regression
newx <- seq(min(mpg$displ), max(mpg$displ), length.out=100) # min and max dispertion
preds <- predict(lm.out, newdata = data.frame(displ=newx), 
                 interval = 'confidence') # Extract confidence interval value
plot(hwy~displ, data = mpg, bg = "black", col = "black", pch =21) # Plot
polygon(c(rev(newx), newx), 
        c(rev(preds[ ,3]), preds[ ,2]), 
        col = 'grey80', border = NA) # add predicted values
abline(lm.out) # plot the linear regression
lines(newx, preds[ ,3], lty = 'dashed', col = 'red') # Plot predicted values
lines(newx, preds[ ,2], lty = 'dashed', col = 'red')

#------------------------------------------------------------#
#Boxplot with significant value
#------------------------------------------------------------#
#*GGplot2*
ggplot(data = iris, aes(Species, Sepal.Length)) + 
  geom_boxplot()+ labs(title = "Boxplot") +
  geom_signif(comparisons = list(c("versicolor", "virginica")), 
              map_signif_level=TRUE)

#*Base Plot* (optional)
boxplot(iris$Sepal.Length ~ iris$Species) # For analysis of variance 


#------------------------------------------------------------#
# Violin plot
#------------------------------------------------------------#
#*GGplot2*
ggplot(data = iris, aes(Species, Sepal.Length)) +
  geom_violin()+ 
  labs(title = "Violin plot")


#*Base Plot* (optional)
if(!require(vioplot)) install.packages("vioplot")
if(!require(vioplot)) library(vioplot)

vioplot(iris$Sepal.Length[iris$Species == "setosa"],
        iris$Sepal.Length[iris$Species == "versicolor"],
        iris$Sepal.Length[iris$Species == "virginica"],
        col = "lightblue")

#------------------------------------------------------------#
#Plot with errors bars
#------------------------------------------------------------#
#*GGplot2*
# Data loading
df <- data.frame(
  trt = factor(c(1, 1, 2, 2)),
  resp = c(1, 5, 3, 4),
  group = factor(c(1, 2, 1, 2)),
  upper = c(1.1, 5.3, 3.3, 4.2),
  lower = c(0.8, 4.6, 2.4, 3.6)
)

# GGplot with Error bar
ggplot(df, aes(trt, resp, colour = group)) +
  geom_pointrange(aes(ymin = lower, ymax = upper))+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(title = "Plot with error bars")


#*Base Plot* (optional)
plot(y~x)
error.bar <- function(...)
error.bar(x,y)

#Data loading
x = as.numeric(c(1,2))
y = c(2,3)
mean = mean(y)

#Base plot
plot(y~x,
     type = "p",
     pch =21,
     bg = "black",
     col = "black",
     ylim = c(0,4),
     xlim = c(0,3),
     xaxt="n",
     main = "My graph",
     xlab = "Site Category",
     ylab = "Mean")

#Axis tunning
axis(side = 1, at = c(1,2), labels = levels(c("yo","man")), cex.lab =1, cex.axis =1)

#error Bar function
error.bar <- function(x,y,epsilon = NULL, se = NULL, se.mul = 1,col = "black") {
  x = as.numeric(x)
  if(is.null(se)){
    stderr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
    se = se.mul*stderr(y)
    } else {se = se.mul*se}
  segments(x, y-se,x, y+se,col = col)
  if(is.null(epsilon)){
    epsilon = 0.02} else {epsilon = epsilon}
  segments(x-epsilon,y-se,x+epsilon,y-se,col = col)
  segments(x-epsilon,y+se,x+epsilon,y+se,col = col)
  }

#Add Error bar
error.bar(x,y)


#------------------------------------------------------------#
#Map
#------------------------------------------------------------#
#*GGplot2*
crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
crimesm <- reshape2::melt(crimes, id = 1)
library(maps)
states_map <- map_data("state")
ggplot(crimes, aes(map_id = state)) +
  geom_map(aes(fill = Murder), map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat) + coord_map()+ labs(title = "Map")


#*Base Plot* (optional)
library(mapproj)
library(rworldmap)
newmap <- rworldmap::getMap(resolution = "high")
plot(newmap, xlim = c(-92, -89),
     ylim = c(-1.5, 0.5),
     asp = 1,
     main = "Galapagos islands")


#------------------------------------------------------------#
#Density Graph
#------------------------------------------------------------#
#*GGplot2*
data("diamonds")
ggplot(diamonds, aes(carat)) +geom_density()+ labs(title = "Density graph")


#*Base Plot* (optional)
plot(density(c(-20, rep(0,98), 20)), xlim = c(-4, 4),
main = "Density graph")

#------------------------------------------------------------#
#Dendogram
#------------------------------------------------------------#
#*GGplot2*

if(!require(ggdendro)) install.packages("ggdendro")
if(!require(ggdendro)) library(ggdendro)

USArrests.short = USArrests[1:10,]
hc <- hclust(dist(USArrests.short), "ave")
ggdendrogram(hc, rotate = TRUE, theme_dendro = FALSE) + 
labs(title = "Dendrogram")

#*Base Plot* (optional)
plot(hc, hang = -1, main = "Dendrogram")


#------------------------------------------------------------#
# Geom_point & Geom_line
#------------------------------------------------------------#
CO2.plot.facet.baline <- CO2.plot.facet + geom_line()
CO2.plot.facet.line <- CO2.plot.facet + geom_line(aes(group=Plant))
multiplot(CO2.plot.facet.baline, CO2.plot.facet.line)


#------------------------------------------------------------#
# Final Challenge
#------------------------------------------------------------#
# Dataset : any (recommanded: use your dataset)

data(msleep)
data(OrchardSprays)
data(mtcars)
mtcars.short <- mtcars[1:20,]
mtcars.short.hc <- hclust(dist(mtcars.short), "complete")

# Geom : any
# Aes : any
# Theme : any
# Facet : any
# ggsinif
# multiplot

#------------------------------------------------------------#
# Miscellaneous : `qplot()` vs `ggplot()`
#------------------------------------------------------------#
qplot(data = iris,
x = Sepal.Length,
xlab = "Sepal Length (mm)",
y = Sepal.Width,
ylab = "Sepal Width (mm)",
main = "Sepal dimensions")

ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
geom_point() +
xlab("Sepal Length (mm)") +
ylab("Sepal Width (mm)") +
ggtitle("Sepal dimensions")


#------------------------------------------------------------#
# Miscellaneous : Dynamics plots
#------------------------------------------------------------#
# Libraries
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if (!require(plotly)) install.packages("plotly")
library(plotly)

# Scatterplot
p=ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species, shape=Species)) + 
  geom_point(size=6, alpha=0.6)

ggplotly(p)
# Select the Species data to display directly From the legend

#------------------------------------------------------------#
# Available elements
#------------------------------------------------------------#
#[Data Visualization with ggplot2 Cheat Sheet]
#(https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf)


#------------------------------------------------------------#
# Additional resources
#------------------------------------------------------------#
`help(package = ggplot)`
#http://ggplot2.tidyverse.org/reference/



#------------------------------------------------------------#
# Solution Challenge 1#
#------------------------------------------------------------#
ggplot(data = iris, # Specify iris dataset 
       aes(x = Petal.Length, # Specify X values
           y = Petal.Width)) + # Specify Y values
  geom_point() # Add the geom_point


#------------------------------------------------------------#
# Solution to challenge #2
#------------------------------------------------------------#
## Example using iris database
data("iris") # Load the iris dataset
iris.plot <- ggplot(data = iris, # Add your data
                    aes(x = Petal.Length, # Specify X values
                        y = Petal.Width,  # Specify Y values
                        color = Species)) + # Set color values
  geom_point() # Add the geom_point
iris.plot # plot your ggplot object

## Example using CO2 database
data(CO2)
CO2.plot <- ggplot(data = CO2, aes(x = conc, y = uptake, colour = Treatment)) +
  geom_point()
CO2.plot

# Example using msleep database
data("msleep")
msleep.plot <- ggplot(data = msleep, aes(x = log10(bodywt), y = awake, colour = vore, shape= conservation)) +
  geom_point()
msleep.plot


#------------------------------------------------------------#
# Résultat Challenge #3
#------------------------------------------------------------#
library(reshape2)
tips.gg <- ggplot(tips, aes(x = total_bill,
                            y = tip/total_bill,
                            shape = smoker, 
                            color = sex, 
                            size = size))
tips.gg <- tips.gg + geom_point()  # Add the geom
tips.gg <- tips.gg + facet_grid( ~ time) # Add Facet
tips.gg <- tips.gg + scale_color_manual(values=cbbPalette) # Add Manueal Blind Color friendly Palette
tips.gg <- tips.gg + ggtitle("Tips per total bill at Lunch and Dinner") # Add plot Title
tips.gg <- tips.gg + theme(axis.title.x = element_text(size = 16, colour = cbbPalette[6]), # Set x axis theme values
                           axis.title.y = element_text(size = 16, colour = cbbPalette[6]), # Set y axis theme values
                           axis.text.x = element_text(size = 12), # Set x axis text size
                           axis.text.y = element_text(size = 12), #  Set y axis text size
                           plot.title = element_text(size = 16, colour = cbbPalette[7], face="bold"), #  Set Plot title theme values
                           strip.text.x = element_text(size = 14, face="bold")) # Set x axis values (scale) theme
tips.gg

#------------------------------------------------------------#
# Solution to Final Challenge
#------------------------------------------------------------#
msleep.challenge4 <- ggplot(msleep, aes(vore, log10(brainwt), fill=vore))  
msleep.challenge4 <- msleep.challenge4 + geom_violin()
msleep.challenge4 <- msleep.challenge4 + geom_signif(comparisons = list(c("herbi", "insecti")))
msleep.challenge4 <- msleep.challenge4 + ggtitle("Brain weight among different vore")
msleep.challenge4 <- msleep.challenge4 + ylab("Brain weight (log10(Kg))")
msleep.challenge4 <- msleep.challenge4 + scale_fill_grey() 
msleep.challenge4 <- msleep.challenge4 + theme_classic()
msleep.challenge4 <- msleep.challenge4 + theme(axis.title.x = element_text(size = 16),
                                               axis.title.y = element_text(size = 16),
                                               axis.text.x = element_text(size = 12),
                                               axis.text.y = element_text(size = 12),
                                               plot.title = element_text(size = 16, face="bold"),
                                               legend.title = element_text(size=14, face="bold"),
                                               legend.text = element_text(size=12))
msleep.challenge4         

brain_body.challenge4 <- ggplot(msleep, aes(log10(bodywt), log10(brainwt))) 
brain_body.challenge4 <- brain_body.challenge4 + geom_point()
brain_body.challenge4 <- brain_body.challenge4 + geom_smooth(method = lm)
brain_body.challenge4 <- brain_body.challenge4 + ggtitle("Linear regression on bodywt ~ brainwt")
brain_body.challenge4 <- brain_body.challenge4 + ylab("Brain weight log10(Kg)") + xlab("Body weight log10(Kg)")
brain_body.challenge4 <- brain_body.challenge4 + theme(axis.title.x = element_text(size = 16),
                                                       axis.title.y = element_text(size = 16),
                                                       axis.text.x = element_text(size = 12),
                                                       axis.text.y = element_text(size = 12),
                                                       plot.title = element_text(size = 14, face="bold"),
                                                       legend.title = element_text(size=14, face="bold"),
                                                       legend.text = element_text(size=12))
brain_body.challenge4 


mtcars.short <- mtcars[1:20,]
mtcars.short.hc <- hclust(dist(mtcars.short), "complete")
dendro.challenge4 <- ggdendrogram(mtcars.short.hc, rotate = TRUE, theme_dendro = FALSE)
dendro.challenge4 <- dendro.challenge4 + ggtitle("Car dendro from motor spec")
dendro.challenge4 <- dendro.challenge4 + xlab("Cars")
dendro.challenge4 <- dendro.challenge4 + theme(axis.title.y = element_text(size = 16),
                                               axis.title.x = element_blank(),
                                               axis.text.x = element_blank(),
                                               axis.text.y = element_text(size = 12),
                                               plot.title = element_text(size = 14, face="bold"))
dendro.challenge4 

data(OrchardSprays)
Orchard.challenge4 <- ggplot(data = OrchardSprays, aes(x=treatment, y=decrease)) + geom_boxplot()
Orchard.challenge4 <- Orchard.challenge4 + ggtitle("Decrease of Orchad bees with different treatments")
Orchard.challenge4 <- Orchard.challenge4 + ylab("Orchad bees decrese")
Orchard.challenge4 <- Orchard.challenge4 + theme(axis.title.y = element_text(size = 14),
                                                 axis.title.x = element_text(size = 14),
                                                 axis.text.x = element_text(size = 12),
                                                 axis.text.y = element_text(size = 12),
                                                 plot.title = element_text(size = 14, face="bold"))
Orchard.challenge4


multiplot(msleep.challenge4,dendro.challenge4, brain_body.challenge4, Orchard.challenge4, cols = 2)

