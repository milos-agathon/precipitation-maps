# 1. PACKAGES

if(
    !require("pacman")
){
    install.packages("pacman")
}

pacman::p_load(
    pRecipe,
    giscoR,
    terra,
    tidyverse,
    rayshader,
    sf,
    classInt
)

# 2. COUNTRY EXTENT

country_sf <- giscoR::gisco_get_countries(
    country = "CH",
    resolution = "1"
)

# 3. PRECIPATION DATA

pRecipe::download_data(
    dataset = "mswep",
    path = getwd(),
    domain = "raw",
    timestep = "yearly"
)

list.files()

mswep_data <- terra::rast(
    "mswep_tp_mm_global_197902_202301_025_yearly.nc"
) |>
terra::crop(
    country_sf
)

terra::plot(mswep_data[[1]])
plot(sf::st_geometry(country_sf), add = TRUE)

# 4. PANEL PRECIPITATION

names(mswep_data) <- 1979:2023

mswep_df <- mswep_data |>
    as.data.frame(xy = TRUE) |>
    tidyr::pivot_longer(
        !c("x", "y"),
        names_to = "year",
        values_to = "precipitation"
    ) |>
    dplyr::filter(year != 2023)

head(mswep_df)

# 5. THEME, BREAKS, COLORS

theme_for_the_win <- function(){
    theme_minimal() +
    theme(
        axis.line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "right",
        legend.title = element_text(
            size = 11, color = "grey10"
        ),
        legend.text = element_text(
            size = 10, color = "grey10"
        ),
        panel.grid.major = element_line(
            color = NA
        ),
        panel.grid.minor = element_line(
            color = NA
        ),
        plot.background = element_rect(
            fill = NA, color = NA
        ),
        legend.background = element_rect(
            fill = "white", color = NA
        ),
        panel.border = element_rect(
            fill = NA, color = NA
        ),
        plot.margin = unit(
            c(
                t = 0, r = 0,
                b = 0, l = 0
            ), "lines"
        )
    )
}

breaks <- classInt::classIntervals(
    mswep_df$precipitation,
    n = 5,
    style = "equal"
)$brks

colors <- hcl.colors(
    n = length(breaks),
    palette = "Temps",
    rev = TRUE
)

# 6. 2D PANEL MAP

map1 <- ggplot(
    data = mswep_df
) +
geom_raster(
    aes(
        x = x,
        y = y,
        fill = precipitation
    )
) +
geom_contour(
    aes(
       x = x,
       y = y,
       z = precipitation 
    ), color = "white" # add this line
) +
geom_sf(
    data = country_sf,
    fill = "transparent",
    color = "grey10",
    size = .5
) +
scale_fill_gradientn(
    name = "mm",
    colors = colors,
    breaks = breaks,
    labels = round(breaks, 0), # use round(breaks, 0)
    limits = c(
        min(mswep_df$precipitation),
        max(mswep_df$precipitation)
    )
) +
facet_wrap(~year) +
guides(
    fill = guide_colourbar(
        direction = "vertical",
        barheight = unit(50, "mm"),
        barwidth = unit(5, "mm"),
        title.position = "top",
        label.position = "right",
        title.hjust = .5,
        label.hjust = .5,
        ncol = 1,
        byrow = FALSE
    )
) +
theme_for_the_win()

# 7. AVERAGE PRECIPATION

mswep_average_df <- mswep_df |>
    dplyr::group_by(
        x, y, .drop = FALSE
    ) |>
    dplyr::summarise(
        mean = mean(precipitation)
    )

head(mswep_average_df)

breaks <- classInt::classIntervals(
    mswep_average_df$mean,
    n = 5,
    style = "equal"
)$brks

colors <- hcl.colors(
    n = length(breaks),
    palette = "Temps",
    rev = TRUE
)

map2 <- ggplot(
    data = mswep_average_df
) +
geom_raster(
    aes(
        x = x,
        y = y,
        fill = mean
    )
) +
geom_contour(
    aes(
       x = x,
       y = y,
       z = mean 
    ), color = "white" # add this line
) +
geom_sf(
    data = country_sf,
    fill = "transparent",
    color = "grey10",
    size = .5
) +
scale_fill_gradientn(
    name = "mm",
    colors = colors,
    breaks = breaks, 
    labels = round(breaks, 0), # use round(breaks, 0)
    limits = c(
        min(mswep_average_df$mean),
        max(mswep_average_df$mean)
    )
) +
guides(
    fill = guide_colourbar(
        direction = "vertical",
        barheight = unit(50, "mm"),
        barwidth = unit(5, "mm"),
        title.position = "top",
        label.position = "right",
        title.hjust = .5,
        label.hjust = .5,
        ncol = 1,
        byrow = FALSE
    )
) +
theme_for_the_win()

# 8. 3D CONTOUR MAP

rayshader::plot_gg(
    ggobj = map2,
    width = 7,
    height = 7,
    scale = 250,
    solid = FALSE,
    shadow = TRUE,
    shadowcolor = "white",
    shadowwidth = 0,
    shadow_intensity = 1,
    window.size = c(600, 600),
    zoom = .7,
    phi = 85,
    theta = 0
)

rayshader::render_camera(
    phi = 60,
    theta = 30    
)

# 9. RENDER OBJECT
#-----------------

u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/air_museum_playground_4k.hdr"

hdri_file <- basename(u)

download.file(
    url = u,
    destfile = hdri_file,
    mode = "wb"
)

rayshader::render_highquality(
    filename = "switzerland-average-precipitation.png",
    preview = TRUE,
    interactive = FALSE,
    parallel = TRUE,
    light = TRUE,
    environment_light = hdri_file,
    intensity = .45,
    rotate_env = 90,
    width = 2000,
    height = 2000
)
