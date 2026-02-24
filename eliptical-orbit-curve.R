library(ggplot2)
library(animint2)
library(dplyr)

# SIMULATION
totalsteps <- 300
dt <- 0.1 
GM <- 5   

x <- 5     
y <- 0
vx <- 0    
vy <- 0.8  

simulationdata <- data.frame()

for (t in 1:totalsteps) {
  rsq <- x^2 + y^2
  r <- sqrt(rsq)
  forcemag <- GM / rsq
  ax <- -forcemag * (x / r)
  ay <- -forcemag * (y / r)
  vx <- vx + ax * dt
  vy <- vy + ay * dt
  x <- x + vx * dt
  y <- y + vy * dt
  currentspeed <- sqrt(vx^2 + vy^2)
  
  simulationdata <- rbind(simulationdata, data.frame(
    timestep = t,
    xpos = x,
    ypos = y,
    distance = r,
    speed = currentspeed
  ))
}

# Creating plot
plotellipse <- ggplot() +
  geom_point(aes(x=0, y=0), color="orange", size=5) +
  geom_path(data = simulationdata, aes(x = xpos, y = ypos), 
            color = "grey", size = 1, alpha=0.5) +
  geom_point(data = simulationdata, 
             aes(x = xpos, y = ypos), 
             color = "blue", size = 4,
             showSelected = "timestep") +
  coord_fixed() +
  labs(title = "Elliptical Orbit Simulation", x = "X", y = "Y") +
  theme_minimal()

plotspeed <- ggplot() +
  geom_tallrect(data = simulationdata,
                aes(xmin = timestep - 0.5, xmax = timestep + 0.5),
                clickSelects = "timestep",
                alpha = 0.1, fill = "blue") +
  geom_line(data = simulationdata, aes(x = timestep, y = speed), color = "red") +
  geom_point(data = simulationdata, aes(x = timestep, y = speed), 
             color = "blue", showSelected = "timestep") +
  labs(title = "Speed Variation", x = "Time", y = "Speed") +
  theme_minimal()

viz <- animint(
  orbit = plotellipse,
  speedgraph = plotspeed,
  time = list(variable = "timestep", ms = 100),
  title = "Elliptical motion",
  source = "https://github.com/NandaniAggarwal/R/blob/main/eliptical-orbit-curve.R"
)

viz
