library(ggplot2)
library(tidyr)
library(extrafont)

# theme_set(theme_bw())
set.seed(1)
x = 1:100
y1 = x*rnorm(length(x),2,4)^2
y2 = x*rnorm(length(x),4,6)^2
y3 = x*rnorm(length(x),6,8)^2
ym= matrix(c(y1,y2,y3), nrow = length(x), ncol = 3)
df =data.frame(ym,x )
dff=tidyr::gather(data = df,value = mea,key = var, X1:X3)
dff$mlty = c(rep(1,length(x)),rep(2,length(x)),rep(3,length(x)))
dff$mea = abs(dff$mea)
ob1 = ggplot(data= dff, aes(x = x,
                      y = mea,
                      colour = var)) + 
  geom_point(aes(shape = as.factor(mlty))) + 
  geom_line(aes(linetype=as.factor(mlty), 
                color=var)) +
  geom_smooth(method = "loess",se = F) + 
  coord_trans(y="log10") +
  # scale_y_log10() +
  ggtitle(label = "experimental", 
          subtitle = "first test") + 
  labs(x = "", 
       y = "change") + 
  scale_colour_manual(values =c("springgreen4",
                                "springgreen2",
                                "lightgreen"))
ob2= ob1 + theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    panel.grid.major = element_line(colour = "lightsteelblue1", 
        linetype = "dotted"), panel.grid.minor = element_line(colour = "lightsteelblue1"), 
    axis.text = element_text(face = "italic", 
        colour = "lightblue4"), axis.text.x = element_text(family = "serif"), 
    legend.title = element_text(family = "serif"), 
    panel.background = element_rect(fill = "red2"), 
    plot.background = element_rect(fill = "aliceblue"), 
    legend.key = element_rect(fill = "lightblue1"), 
    legend.background = element_rect(fill = "lightcyan3"), 
    legend.position = "bottom", legend.direction = "horizontal",
    text=element_text(size=16,  
                       family="Comic Sans MS"))
ob3 = ob2 + labs(caption  =  "3 different trials of the same experiment. Used potatoes to try something instead of old technique. Clearly works better.")
print(ob3)
#+
  # facet_wrap(~var)