# ============================================================
# Compute DAILY DTR from tasmax/tasmin and CLIP to Pennsylvania
# PA boundary pulled programmatically (no shapefile needed)
# ============================================================

suppressPackageStartupMessages({
  library(terra)
  library(lubridate)
  library(tigris)
})

# ----------------------------
# 1) OPEN TASMAX/TASMIN
# ----------------------------
tasmax <- rast("tasmax_day_ACCESS-CM2_ssp126_r1i1p1f1_gn_2050_v2.0.nc")
tasmin <- rast("tasmin_day_ACCESS-CM2_ssp126_r1i1p1f1_gn_2050_v2.0.nc")

stopifnot(nlyr(tasmax) == nlyr(tasmin))
stopifnot(all(time(tasmax) == time(tasmin)))

# ----------------------------
# 2) DTR
# ----------------------------
dtr <- tasmax - tasmin

# If the raster uses 0..360 longitude, rotate to -180..180 BEFORE clipping
if (is.lonlat(dtr)) {
  e <- ext(dtr)
  if (e[1] >= 0 && e[2] > 180) {
    dtr <- rotate(dtr)
  }
}

# ----------------------------
# 3) GET PENNSYLVANIA BOUNDARY
# ----------------------------
pa <- vect(states(cb = TRUE))
pa <- pa[pa$NAME == "Pennsylvania", ]
pa <- aggregate(pa)

# Match CRS after any rotation
if (!same.crs(pa, dtr)) {
  pa <- project(pa, crs(dtr))
}

# Quick overlap check
print(ext(dtr))
print(ext(pa))

# ----------------------------
# 4) CROP + MASK TO PA
# ----------------------------
dtr_pa <- mask(crop(dtr, pa), pa)

# ----------------------------
# 5) DAILY PA MEAN DTR
# ----------------------------
pa_daily <- global(dtr_pa, mean, na.rm = TRUE)
names(pa_daily)[1] <- "DTR_PA_mean"
pa_daily$date <- as.Date(time(dtr_pa))

# Annual mean DTR (°C)
mean(pa_daily$DTR_PA_mean, na.rm = TRUE)

head(pa_daily)

# ----------------------------
# 6) PLOT
# ----------------------------
plot(pa_daily$date, pa_daily$DTR_PA_mean, type = "l",
     xlab = "Date",
     ylab = "DTR (°C)",
     main = "Pennsylvania Daily Mean DTR (ACCESS-CM2, SSP1-2.6, 2050)")
