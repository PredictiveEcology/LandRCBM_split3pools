gg_agbpools <- function(x, title) {
  gg <- ggplot() +
    tidyterra::geom_spatraster(data = x) +
    viridis::scale_fill_viridis(na.value = "transparent") +
    ggtitle(title) + theme_bw()
}

gg_landscapesummary <- function(x) {
  d <- as.data.table(x) |>
    melt(id.vars = "year", variable.name = "pool", value.name = "biomass")
  gg <- ggplot(d) + 
    geom_line(aes(x = year, color = pool, y= biomass)) +
    ggtitle("AGB by pool across pixels") +
    labs(x = "Year", y = "Biomass") +
    theme_bw()
  
}

gg_speciessummary <- function(x) {
  d <- as.data.table(x) |>
    melt(id.vars = c('species', "year"), variable.name = "pool", value.name = "biomass")
  gg <- ggplot(d) + 
    geom_line(aes(x = year, color = pool, y= biomass)) + 
    facet_wrap(~species) +
    ggtitle("AGB by pool and species across pixels") +
    labs(x = "Year", y = "Biomass") +
    theme_bw()
}

gg_yieldCurves <- function(x, title){
  ggplot(x, aes(age, B, color = speciesCode)) + geom_line() + theme_bw() +
    facet_wrap(~yieldPixelGroup)
}

gg_yieldCurvesPools <- function(x, title) {
  ggplot(x, aes(age, B, fill = pool)) +
    geom_area(position = position_stack()) +
    theme_bw() +
    facet_grid(species ~ yieldPixelGroup) +
    ggtitle(title) +
    theme(panel.background = element_rect(fill = "white", color = NA),
          panel.grid = element_blank())
}

gg_yieldCurves <- function(x, title){
  ggplot(x, aes(age, B, color = speciesCode)) + geom_line() + theme_bw() +
    facet_wrap(~yieldPixelGroup)
}
