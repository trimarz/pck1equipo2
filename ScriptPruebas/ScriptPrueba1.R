####################################################################################################################################################3
# Inicializaciones varias:
#    seed :
#    scope: número de registros para la muestra
#
#
###############################################################################################################################################3çç
verbose <- TRUE
seed <- 666
scope <- 500
output.file <- "geoftps.rds" # ????? no se usa
library("pck1equipo2")

# Initial Setup
if (verbose) print("[*] Initial setup")
tini <- Sys.time()
set.seed(666)
dir.data <- file.path(getwd(), "data")
if (!dir.exists(dir.data)) {
  if (verbose) print("[*] Create data directory")
  dir.create(dir.data)
}


####################################################################################################################################################3
# Dataset   : escaneo de puertos UDP con la herramienta ZMAP
#             2019-05-06-1557142645-udp_mssql_1434.csv.gz
#
# URL       : https://opendata.rapid7.com/sonar.udp/2019-05-06-1557142645-udp_mssql_1434.csv.gz
###############################################################################################################################################3
udpscans.source <- file.path(getwd(), "data","2019-04-29-1556560516-gtp-c_2123.csv")
udpscans.file.gz <- paste(udpscans.source, ".gz", sep = "")
#ReadRawData("https://opendata.rapid7.com/sonar.udp/2019-04-29-1556560516-gtp-c_2123.csv.gz", udpscans.file.gz, dir.data, udpscans.source, TRUE)
DownloadData("https://opendata.rapid7.com/sonar.udp/2019-04-29-1556560516-gtp-c_2123.csv.gz", udpscans.file.gz, TRUE)
R.utils::gunzip(udpscans.file.gz)
udp.df <- read.csv(udpscans.source)
print(dp.df)

####################################################################################################################################################
# Seleccionamos una muestra de scans
####################################################################################################################################################
muestra <- sample(1:nrow(udp.df), scope)
udp.df.muestra <- udp.df[muestra,]
rm(muestra)


####################################################################################################################################################3
# Dataset   : geolocalización de un rango de IP
#             GeoLite2-City-CSV.zip
#
# URL       : https://geolite.maxmind.com/download/geoip/database/GeoLite2-City-CSV.zip
###############################################################################################################################################3
maxmind.file <- file.path(getwd(), "data", "maxmind.zip")
DownloadData("https://geolite.maxmind.com/download/geoip/database/GeoLite2-City-CSV.zip", maxmind.file, TRUE)
csv.file.in.zip <- "GeoLite2-City-Blocks-IPv4.csv"
unzip(zipfile = maxmind.file, exdir = csv.file.in.zip)

csv.unzipped.file <- "GeoLite2-City-CSV_20190528/GeoLite2-City-Locations-zh-CN.csv"
df.maxmind <- read.csv(csv.unzipped.file, stringsAsFactors = FALSE)
print(df.maxmind)



####################################################################################################################################################3
# Crea columnas con la IP max y la IP min
####################################################################################################################################################3
if (verbose) print("[*] Crea columna con IP max e IP min")
print("print df maxmind")

print(df.maxmind)
my.ip.range <- df.maxmind$network
print("print my.range3")
print(my.ip.range)
# my.range <- iptools::range_boundaries(df.maxmind$network)
my.range <- iptools::range_boundaries(my.ip.range)
df.maxmind <- cbind(df.maxmind, iptools::range_boundaries(df.maxmind$network))
df.maxmind$rowname <- as.integer(row.names(df.maxmind))

# Usamos multiples cpu's para geolocalizar IPs en rangos
if (verbose) print("[*] Foreach IP (source and destination) identify network range using parallel computing")
no_cores <- parallel::detectCores() - 1
cl <- parallel::makeCluster(no_cores)
parallel::clusterExport(cl, "df.maxmind")
udp.df.muestra$sloc <- sapply(udp.df.muestra$saddr.num,
                              function(ip)
                                which((ip >= df.maxmind$min_numeric) &
                                        (ip <= df.maxmind$max_numeric)))
udp.df.muestra$dloc <- sapply(udp.df.muestra$daddr.num,
                              function(ip)
                                which((ip >= df.maxmind$min_numeric) &
                                        (ip <= df.maxmind$max_numeric)))
parallel::stopCluster(cl)
rm(cl, no_cores)

# Join and tidy data frame (destination addre
ss)
if (verbose) print("[*] Joining destination IP's with geolocation data")
suppressMessages(library(dplyr))
df.dst <- udp.df.muestra %>%
  left_join(df.maxmind, by = c("dloc" = "rowname")) %>%
  select(daddr, latitude, longitude)
names(df.dst) <- c("daddr", "dlatitude", "dlongitude")
df <- dplyr::bind_cols(df, df.dst)
write.csv(udp.df.muestra, file = "udp.df.muestra.csv")

rm(df.dst, udp.df.muestra)


# Set categoric variables as factors
if (verbose) print("[*] Tidy data and save it")
df$is_anonymous_proxy <- as.factor(df$is_anonymous_proxy)
df$is_satellite_provider <- as.factor(df$is_satellite_provider)
saveRDS(object = df, file = file.path(getwd(), "data", output.file))
fini <- Sys.time()

# Summary
fini - tini
write.csv(df, file = "df.csv")

summary(df)####################################################################################################################################################
# Representación gráfica
####################################################################################################################################################
p <- ggplot(df, aes(x = Petal.Length, y = Petal.Width, colour = Species))




