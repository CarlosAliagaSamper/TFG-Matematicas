library(sf)
library(gstat)
library(sp)

# Directorio de los datos (cambiar en otros ordenadores)
setwd("C:/Users/Usuario/Dropbox/TFG Matematicas Carlos Aliaga/Codigo de R/data")

# Lectura de los datos (urban suburban background)
df <- st_read("st_usb_2019_pm10.shp")

# Limpieza previa a poder trabajar
PM10_avg19_df <- df %>% st_drop_geometry()

# Eliminar repetidos en EoICode
PM10_avg19_df <- PM10_avg19_df[-1037,]

# Calculos
gdf <- gstat(g = NULL,
             id = "PM10_avg19",
             formula = PM10_avg19~1,
             locations = ~etrs_X+etrs_Y,
             data = PM10_avg19_df)

mi_cutoff <- 1000000 # metros (1000 km)
lags <- 15

vemp <- variogram(object = gdf,
                  cutoff = mi_cutoff,
                  width = mi_cutoff/lags,
                  cloud = FALSE)
plot(vemp, cutoff = mi_cutoff)
vemp.fit_sph <- fit.variogram(vemp, model = vgm("Sph"))
vemp.fit_exp <- fit.variogram(vemp, model = vgm("Exp"))
# Estos no convergen (se descartan)
# vemp.fit_wav <- fit.variogram(vemp, model = vgm("Wav"))
# vemp.fit_pow <- fit.variogram(vemp, model = vgm("Pow"))

plot(vemp, vemp.fit_sph, cutoff = mi_cutoff)
plot(vemp, vemp.fit_exp, cutoff = mi_cutoff)
# plot(vemp, vemp.fit_wav, cutoff = mi_cutoff)
# plot(vemp, vemp.fit_pow, cutoff = mi_cutoff)

(sse.sph <- attr(vemp.fit_sph, "SSErr"))
(sse.exp <- attr(vemp.fit_exp, "SSErr"))
# (sse.wav <- attr(vemp.fit_wav, "SSErr"))
# (sse.pow <- attr(vemp.fit_pow, "SSErr"))

vemp.fit <- vemp.fit_sph
plot(vemp, vemp.fit, cutoff = mi_cutoff)

# Grid
setwd("C:/Users/Usuario/Downloads/Interpolated-data-2km-grid")
grid <- st_read("EEA_2kmgrid_2019.shp")
grid <- grid %>% st_drop_geometry()
coordinates(grid) <- ~POINT_X+POINT_Y

# Kriging
coordinates(PM10_avg19_df) <- ~etrs_X+etrs_Y
vemp.kriged <- krige(PM10_avg19~1,
                     PM10_avg19_df,
                     newdata = grid,
                     model = vemp.fit)

# Exportar los resultados (cambiar en otros ordenadores)
setwd("C:/Users/Usuario/Downloads/resultados")

vemp.kriged <- vemp.kriged %>% st_drop_geometry()
write.csv(vemp.kriged, file = "resultados.csv")