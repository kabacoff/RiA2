#-----------------------------------------------------------#
# R in Action (2nd ed): Chapter 23 - Bonus online chapter   #
# Advanced graphics with the lattice package                #
#-----------------------------------------------------------#

par(ask=TRUE)

## Lattice Package
library(lattice)


# Histogram of heights conditioned on voice pitch
histogram(~ height | voice.part, data = singer,
          main="Distribution of Heights by Voice Pitch",
          xlab="Height (inches)")


# Listing 23.1 - Lattice plot examples
data(mtcars)
attach(mtcars)


# Create factors with value labels 
gear <- factor(gear, levels=c(3, 4, 5),
               labels=c("3 gears", "4 gears", "5 gears")) 

cyl <- factor(cyl, levels=c(4, 6, 8),
              labels=c("4 cylinders", "6 cylinders", "8 cylinders")) 


# Generate plots
densityplot(~ mpg, 
            main="Density Plot", 
            xlab="Miles per Gallon")

densityplot(~ mpg | cyl, 
            main="Density Plot by Number of Cylinders",
            xlab="Miles per Gallon")

bwplot(cyl ~ mpg | gear,
       main="Box Plots by Cylinders and Gears",
       xlab="Miles per Gallon", ylab="Cylinders") 

xyplot(mpg ~ wt | cyl * gear, 
       main="Scatter Plots by Cylinders and Gears", 
       xlab="Car Weight", ylab="Miles per Gallon")

cloud(mpg ~ wt * qsec | cyl, 
      main="3D Scatter Plots by Cylinders") 

dotplot(cyl ~ mpg | gear, 
        main="Dot Plots by Number of Gears and Cylinders",
        xlab="Miles Per Gallon")

splom(mtcars[c(1, 3, 4, 5, 6)], 
      main="Scatter Plot Matrix for mtcars Data")

detach(mtcars)


# Manipulating a graph
mygraph <- densityplot(~height|voice.part, data=singer)
plot(mygraph)
update(mygraph, col="red", pch=16, cex=.8, jitter=.05, lwd=2)
plot(mygraph)


# Conditioning on a continuous variable
displacement <- equal.count(mtcars$disp, number=3, overlap=0)
xyplot(mpg ~ wt | displacement, data=mtcars, 
       main = "Miles per Gallon vs. Weight by Engine Displacement",
       xlab = "Weight", ylab = "Mile per Gallon",
       layout=c(3,1), aspect=1.5)


# Listing 23.2 - xyplot with custom panel functions
displacement <- equal.count(mtcars$disp, number=3, overlap=0)

mypanel <- function(x, y) {
  panel.xyplot(x, y, pch=19)                      
  panel.rug(x, y)                                
  panel.grid(h=-1, v=-1)                           
  panel.lmline(x, y, col="red", lwd=1, lty=2)     
}

xyplot(mpg ~ wt|displacement, data=mtcars, 
       layout=c(3, 1), 
       aspect=1.5,
       main = "Miles per Gallon vs. Weight by Engine Displacement",
       xlab = "Weight",
       ylab = "Mile per Gallon",
       panel = mypanel)


# Listing 23.3 - xyplot with custom panel functions and additional options
mtcars$transmission <- factor(mtcars$am, levels=c(0, 1), 
                              labels=c("Automatic",  "Manual"))

panel.smoother <- function(x, y) {
  panel.grid(h=-1, v=-1)
  panel.xyplot(x, y) 
  panel.loess(x, y)
  panel.abline(h=mean(y), lwd=2, lty=2, col="green")
}

xyplot(mpg ~ disp|transmission, data=mtcars,
       scales=list(cex=.8, col="red"),
       panel=panel.smoother,
       xlab="Displacement", ylab="Miles per Gallon", 
       main="MGP vs Displacement by Transmission Type",
       sub = "Dotted lines are Group Means", aspect=1)


# Grouping variables
mtcars$transmission <- factor(mtcars$am, levels=c(0, 1),
                              labels=c("Automatic", "Manual"))
densityplot(~ mpg, data=mtcars,
            group=transmission,
            main="MPG Distribution by Transmission Type",
            xlab="Miles per Gallon",
            auto.key=TRUE)

# Listing 23.4 - Kernel density plot with a group variable and customized legend
mtcars$transmission <- factor(mtcars$am, levels=c(0,1),
                              labels=c("Automatic", "Manual"))

colors = c("red", "blue")  
lines = c(1, 2)
points = c(16, 17)

key.trans <- list(title="Trasmission",  
                  space="bottom", columns=2,
                  text=list(levels(mtcars$transmission)),
                  points=list(pch=points, col=colors),
                  lines=list(col=colors, lty=lines),
                  cex.title=1, cex=.9)

densityplot(~ mpg, data=mtcars,
            group=transmission,
            main="MPG Distribution by Transmission Type",
            xlab="Miles per Gallon",
            pch=points, lty=lines, col=colors,    
            lwd=2, jitter=.005,                                                  
            key=key.trans) 


# Listing 23.5 - xyplot with group and conditioning variables and customized legend
colors <- "darkgreen"
symbols <- c(1:12)
linetype <- c(1:3)

key.species <- list(title="Plant",
                    space="right",
                    text=list(levels(CO2$Plant)), 
                    points=list(pch=symbols, col=colors))

xyplot(uptake ~ conc | Type*Treatment, data=CO2,
       group=Plant, 
       type="o", 
       pch=symbols, col=colors, lty=linetype,
       main="Carbon Dioxide Uptake\nin Grass Plants",
       ylab=expression(paste("Uptake ", 
                             bgroup("(", italic(frac("umol","m"^2)), ")"))),
       xlab=expression(paste("Concentration ", 
                             bgroup("(", italic(frac(mL,L)), ")"))),
       sub = "Grass Species: Echinochloa crus-galli",
       key=key.species)


# Graphical parameters
show.settings()
mysettings <- trellis.par.get()
mysettings$superpose.symbol
mysettings$superpose.symbol$pch <- c(1:10)
trellis.par.set(mysettings)
show.settings()


# Customizing plot strips
histogram(~height | voice.part, data = singer,
          strip = strip.custom(bg="lightgrey",   
                               par.strip.text=list(col="black", cex=.8, font=3)),
          main="Distribution of Heights by Voice Pitch",
          xlab="Height (inches)")

mysettings <- trellis.par.get()
mysettings$strip.background <- c("lightgrey", "lightgreen")
trellis.par.set(mysettings)


# Page arrangement
graph1 <- histogram(~ height | voice.part, data=singer,
                    main="Heights of Choral Singers by Voice Part")
graph2 <- bwplot(height~voice.part, data=singer)
plot(graph1, split=c(1, 1, 1, 2))
plot(graph2, split=c(1, 2, 1, 2), newpage=FALSE)

library(lattice)
graph1 <- histogram(~ height | voice.part, data=singer,
                    main="Heights of Choral Singers by Voice Part")
graph2 <- bwplot(height~voice.part, data=singer)
plot(graph1, position=c(0, .3, 1, 1))
plot(graph2, position=c(0, 0, 1, .3), newpage=FALSE)
