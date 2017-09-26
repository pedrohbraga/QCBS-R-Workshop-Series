# QCBS R workshops
# Plot demo

co2=read.csv('co2_good.csv')

#### scatter plot ####
plot(co2$uptake~co2$conc)
# customize axis labels
plot(co2$uptake~co2$conc,xlab='Concentration',ylab='Uptake',main='Concentration vs uptake',cex.lab=1.5)
        # cex.lab: axis label size
        # main: main title
        # xlab, ylab: x-axis and y-axis labels

# customize plotting window
par(mar=c(5,5,4,2)) # margin: bottom, left, top, right (default: 5,4,4,2)
plot(co2$uptake~co2$conc,xlab='Concentration',ylab='Uptake',main='Concentration vs uptake',cex.lab=1.5)

# customize axis scale
plot(co2$uptake~co2$conc,xlab='Concentration',ylab='Uptake',main='Concentration vs uptake',cex.lab=1.5,
     ylim=c(0,50))

# change apprearence of points
plot(co2$uptake~co2$conc,xlab='Concentration',ylab='Uptake',main='Concentration vs uptake',cex.lab=1.5,
     ylim=c(0,50),pch=20) # pch: define point type
#customize axis
plot(co2$uptake~co2$conc,xlab='Concentration',ylab='Uptake',main='Concentration vs uptake',cex.lab=1.5,
     ylim=c(0,50),pch=20,axes=F) # axes=F: no axis drawn
axis(1,at=c(0,100,200,300,400,500,600,700,800,900,1000),cex.axis=1.2) # 1: x-axis, 
                      # at: position of tick-marks
                      # cex.axis: tick-label size
axis(2,las=2,cex.axis=1.2) # 2: y-axis, 
              # las: orientation of tick-labels 
box() # draw a box around the plot

# add a line 
abline(v=450,col='red',lty=2,lwd=3) # plot command must be called before
                      # col: color (for points, lines, ...)
                      # lty: line type
                      # lwd: line width (thickness)

# add a legend
legend(100,50,c('Data points','Atmospheric concentration'),col=c('black','red'),pch=c(20,-1),lty=c(0,2),lwd=2,bty='n')
  # 0,6: x and y position of the legend box
  # c('Data points','1:1 line'): legend text 
  # col=c('black','red'): symbol colors
  # pch=c(20,-1): point type (-1 means no point drawn)
  # lty=c(0,1): line type (0 means no line drawn)
  # lwd=2: line thickness
  # bty='n': box type ('n' means no box around the legend)

#### histogram ####
chilled=subset(co2,co2$Treatment=='chilled')
nonchilled=subset(co2,co2$Treatment=='nonchilled')

hist(chilled$uptake,main='Uptake chilled plants',nclass=25,xlab='Uptake',cex.lab=1.5)
  # nclass: number of size classes
hist(chilled$uptake,main='Uptake chilled plants',breaks=seq(0,45,by=2),xlab='Uptake',cex.lab=1.5)
  # breaks: define each size class

#### bar chart ####
barplot(c(mean(chilled$uptake),mean(nonchilled$uptake)),ylim=c(0,35),
        names.arg = c('chilled','nonchilled'),cex.names=1.5)
  # c(mean(chilled$uptake),mean(nonchilled$uptake)): vector of values to be plotted
  # names.arg: label for each bar
  # cex.names: label sizes

#### box-and-whisker plot ####
boxplot(uptake~Treatment,data=co2)

#### qqplot ####
qqnorm(chilled$uptake,main='Normal Q-Q plot for uptake (chilled plants)')
qqline(chilled$uptake)
