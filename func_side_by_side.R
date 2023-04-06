library(leaflet)
library(htmltools)
library(htmlwidgets)
LeafletSideBySidePlugin <- htmlDependency("leaflet-side-by-side","2.0.0",
                                          src = c(href="https://cdn.jsdelivr.net/gh/digidem/leaflet-side-by-side@2.0.0/"),
                                          script="leaflet-side-by-side.js")

# A function that takes a plugin htmlDependency object and adds
# it to the map. This ensures that however or whenever the map
# gets rendered, the plugin will be loaded into the browser.

registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

leaflet() %>% addTiles() %>%
  setView(lng = 12, lat = 50, zoom = 4) %>%
  # Register leaflet-side-by-side plugin on this map instance
  registerPlugin(LeafletSideBySidePlugin) %>%
  onRender("
        function(el, x) {
          var mylayer1 = L.tileLayer(
            'http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',{
             maxZoom: 18
             }).addTo(this);
          var mylayer2 = L.tileLayer(
          '//stamen-tiles-{s}.a.ssl.fastly.net/watercolor/{z}/{x}/{y}.png',{
             maxZoom: 14
             }).addTo(this);
        L.control.sideBySide(mylayer1, mylayer2).addTo(this);
        }")

