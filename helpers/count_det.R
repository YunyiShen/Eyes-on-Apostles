get_raw_counts <- function(sppdata) {
  date_spp1 <- as.POSIXct(sppdata$Date, format = "%m/%d/%Y", tz = "CDT")
  years_spp1 <- as.numeric(format(date_spp1, "%Y"))
  summary(as.factor(years_spp1))
}

get_count <- function(spp1, compare, spp2) {
  rawdata <- read.csv("data/CleanEvents.csv")
  rawdata <- rawdata[rawdata$Date != "", ]
  date_all <- as.POSIXlt(rawdata$Date, format = "%m/%d/%Y", tz = "CDT")
  years <- as.numeric(format(date_all, "%Y"))
  julian_date <- as.numeric(format(date_all, "%j"))

  camera_days <- sapply(sort(unique(years)), function(year, years, julian_date) {
    c(year, length(unique(julian_date[years == year])))
  }, years, julian_date) %>%
    t() %>%
    data.frame()
  colnames(camera_days) <- c("year", "camera_days")

  spp_1_data <- rawdata[rawdata$Species == spp1, ]
  if (compare) spp_2_data <- rawdata[rawdata$Species == spp2, ]
  rm(rawdata)
  rm(date_all, years, julian_date)
  n_detections1 <- get_raw_counts(spp_1_data)
  camera_days$spp1 <- n_detections1[as.character(camera_days$year)]
  # camera_days$spp1rai <- n_detections1[as.character(camera_days$year)]/camera_days$camera_days * 100

  # plots <- list()



  if (compare) {
    n_detections2 <- get_raw_counts(spp_2_data)
    camera_days$spp2 <- n_detections2[as.character(camera_days$year)]
    # camera_days$spp2rai <- n_detections2[as.character(camera_days$year)]/camera_days$camera_days * 100

    plot_data_rawcount <- melt(camera_days[, c(1, 3, 4)], id.vars = "year")
    spp_raw <- c("spp1" = spp1, "spp2" = spp2)

    plot_data_rawcount$species <- spp_raw[plot_data_rawcount$variable]
    ggplot(plot_data_rawcount, aes(x = year, y = value, col = species)) +
      geom_smooth() +
      geom_point() +
      ylab("Raw counts")
  } else {
    ggplot(data = camera_days, aes(x = year, y = spp1)) +
      geom_smooth() +
      geom_point() +
      ylab(paste(spp1, "raw detection counts"))
  }
}

get_RAI <- function(spp1, compare, spp2) {
  rawdata <- read.csv("data/CleanEvents.csv")
  rawdata <- rawdata[rawdata$Date != "", ]
  date_all <- as.POSIXlt(rawdata$Date, format = "%m/%d/%Y", tz = "CDT")
  years <- as.numeric(format(date_all, "%Y"))
  julian_date <- as.numeric(format(date_all, "%j"))

  camera_days <- sapply(sort(unique(years)), function(year, years, julian_date) {
    c(year, length(unique(julian_date[years == year])))
  }, years, julian_date) %>%
    t() %>%
    data.frame()
  colnames(camera_days) <- c("year", "camera_days")

  spp_1_data <- rawdata[rawdata$Species == spp1, ]
  if (compare) spp_2_data <- rawdata[rawdata$Species == spp2, ]
  rm(rawdata)
  rm(date_all, years, julian_date)
  n_detections1 <- get_raw_counts(spp_1_data)
  # camera_days$spp1 <- n_detections1[as.character(camera_days$year)]
  camera_days$spp1rai <- n_detections1[as.character(camera_days$year)] / camera_days$camera_days * 100


  if (compare) {
    n_detections2 <- get_raw_counts(spp_2_data)
    # camera_days$spp2 <- n_detections2[as.character(camera_days$year)]
    camera_days$spp2rai <- n_detections2[as.character(camera_days$year)] / camera_days$camera_days * 100

    plot_data_rai <- melt(camera_days[, c(1, 3, 4)], id.vars = "year")
    spp_rai <- c("spp1rai" = spp1, "spp2rai" = spp2)

    plot_data_rai$species <- spp_rai[plot_data_rai$variable]

    ggplot(plot_data_rai, aes(x = year, y = value, col = species)) +
      geom_smooth() +
      geom_point() +
      ylab("RAI")
  } else {
    ggplot(data = camera_days, aes(x = year, y = spp1rai)) +
      geom_smooth() +
      geom_point() +
      ylab(paste(spp1, "RAI"))
  }
}



get_annual_temp <- function(spp1, compare, spp2) {
  rawdata <- read.csv("data/CleanEvents.csv")
  rawdata <- rawdata[rawdata$Date != "", ]


  spp_1_data <- rawdata[rawdata$Species == spp1, ]
  if (compare) spp_2_data <- rawdata[rawdata$Species == spp2, ]
  rm(rawdata)
  date_spp1 <- as.POSIXct(spp_1_data$Date, format = "%m/%d/%Y", tz = "CDT")
  dates_spp1 <- as.numeric(format(date_spp1, "%j"))

  density_data <- data.frame(species = spp1, juliandate = dates_spp1)


  if (compare) {
    date_spp2 <- as.POSIXct(spp_2_data$Date, format = "%m/%d/%Y", tz = "CDT")
    dates_spp2 <- as.numeric(format(date_spp2, "%j"))
    density_data2 <- data.frame(species = spp2, juliandate = dates_spp2)
    density_data <- rbind(density_data, density_data2)

    ggplot(density_data, aes(x = juliandate, col = species)) +
      geom_density() +
      xlab("Julian date")
  } else {
    ggplot(density_data, aes(x = juliandate)) +
      geom_density() +
      xlab("Julian date")
  }
}