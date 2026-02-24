library(ggplot2)
library(animint2)
library(dplyr)

# 1. Setup Planet Info
planetsinfo <- data.frame(
  planetname = c("Mercury", "Venus", "Earth", "Mars"),
  distance = c(0.4, 0.7, 1.0, 1.5),
  pcolor = c("gray", "orange", "blue", "red"), # Alag column for color
  speed = c(4.1, 1.6, 1.0, 0.53) 
)

# 2. Generating Animation Data
numframes <- 200
orbitdata <- data.frame()

for (t in 1:numframes) {
  temp <- planetsinfo %>%
    mutate(
      frameid = t,
      angle = speed * (t / 20), 
      xpos = distance * cos(angle),
      ypos = distance * sin(angle)
    )
  orbitdata <- rbind(orbitdata, temp)
}

# 3. Creating Static Orbit Paths
orbitpaths <- data.frame()
for(i in 1:nrow(planetsinfo)){
  angles <- seq(0, 2*pi, length.out = 100)
  path <- data.frame(
    planetname = planetsinfo$planetname[i],
    xpath = planetsinfo$distance[i] * cos(angles),
    ypath = planetsinfo$distance[i] * sin(angles)
  )
  orbitpaths <- rbind(orbitpaths, path)
}

# 4. Plotting
plotsolarsystem <- ggplot() +
  # Sun
  geom_point(aes(x=0, y=0), color="gold", size=8) +
  # Orbits
  geom_path(data = orbitpaths, aes(x = xpath, y = ypath, group = planetname), 
            color = "grey80", linetype = "dashed") +
  # Moving Planets 
  geom_point(data = orbitdata, 
             aes(x = xpos, y = ypos, color = pcolor), 
             size = 5,
             showSelected = "frameid") +
  # Labels
  geom_text(data = orbitdata,
            aes(x = xpos, y = ypos, label = planetname),
            vjust = -1.5, size = 4,
            showSelected = "frameid") +
  coord_fixed() + 
  scale_color_identity() + 
  labs(title = "Solar System: Orbital Motion",
       subtitle = "Inner planets move faster than outer planets",
       x = "Distance (AU)", y = "Distance (AU)") +
  theme_minimal()


viz <- animint(
  plot = plotsolarsystem,
  time = list(variable = "frameid", ms = 100)
  title = "Planetary orbit motion",
  source = "https://github.com/NandaniAggarwal/R/blob/main/planetary-curve.R"
)

viz
