
#title: "PRAC 2"
#author: "Fabrizio Jesus Caceda Peña"
#date: "2023-06-15"






dataAccidents <- read.csv("./US_Accidents_March23.csv",header=T,sep=",")
dataAccidents <- dataAccidents[format(as.POSIXct(dataAccidents$Start_Time), format = "%Y") != "2023", ]



###############################################################################

#1 Frecuencia de Accidentes de 2016 a 2022
years <- format(as.POSIXct(dataAccidents$Start_Time, format = "%Y-%m-%d %H:%M:%S"), format = "%Y")
years <- years[years != "2023"]
frequency <- as.data.frame(table(years))
colnames(frequency) <- c("Año", "Frecuencia")
resultDataset <- frequency
write.csv(resultDataset, file = "años.csv", row.names = FALSE)


###############################################################################


#2 severidad por años
data2016 <- dataAccidents[format(as.POSIXct(dataAccidents$Start_Time), format = "%Y") == "2016", ]
data2017 <- dataAccidents[format(as.POSIXct(dataAccidents$Start_Time), format = "%Y") == "2017", ]
data2018 <- dataAccidents[format(as.POSIXct(dataAccidents$Start_Time), format = "%Y") == "2018", ]
data2019 <- dataAccidents[format(as.POSIXct(dataAccidents$Start_Time), format = "%Y") == "2019", ]
data2020 <- dataAccidents[format(as.POSIXct(dataAccidents$Start_Time), format = "%Y") == "2020", ]
data2021 <- dataAccidents[format(as.POSIXct(dataAccidents$Start_Time), format = "%Y") == "2021", ]
data2022 <- dataAccidents[format(as.POSIXct(dataAccidents$Start_Time), format = "%Y") == "2022", ]

dfSev16 <- as.data.frame(table(data2016$Severity))
colnames(dfSev16) <- c("Severity", "Frecuencia")


dfSev17 <- as.data.frame(table(data2017$Severity))
colnames(dfSev17) <- c("Severity", "Frecuencia")

dfSev18 <- as.data.frame(table(data2018$Severity))
colnames(dfSev18) <- c("Severity", "Frecuencia")

dfSev19 <- as.data.frame(table(data2019$Severity))
colnames(dfSev19) <- c("Severity", "Frecuencia")

dfSev20<- as.data.frame(table(data2020$Severity))
colnames(dfSev20) <- c("Severity", "Frecuencia")

dfSev21 <- as.data.frame(table(data2021$Severity))
colnames(dfSev21) <- c("Severity", "Frecuencia")

dfSev22 <- as.data.frame(table(data2022$Severity))
colnames(dfSev22) <- c("Severity", "Frecuencia")

tabla_resultado <- merge(dfSev16, dfSev17, by = "Severity", all = TRUE)
tabla_resultado <- merge(tabla_resultado, dfSev18, by = "Severity", all = TRUE)
tabla_resultado <- merge(tabla_resultado, dfSev19, by = "Severity", all = TRUE)
tabla_resultado <- merge(tabla_resultado, dfSev20, by = "Severity", all = TRUE)
tabla_resultado <- merge(tabla_resultado, dfSev21, by = "Severity", all = TRUE)
tabla_resultado <- merge(tabla_resultado, dfSev22, by = "Severity", all = TRUE)

# Renombrar las columnas de los años
colnames(tabla_resultado)[2:8] <- c("2016", "2017", "2018", "2019", "2020", "2021", "2022")

tabla_resultado <- tabla_resultado[order(tabla_resultado$Severity), ]

write.csv(tabla_resultado, "severityByYears.csv", row.names=FALSE) 




################################################################################################################


#accidentes por meses y severidad 3
dataAccidents$Month <- format(as.POSIXct(dataAccidents$Start_Time), format = "%m")

# Crear una tabla vacía para almacenar los resultados
tabla_resultado <- matrix(0, nrow = 12, ncol = 5)
colnames(tabla_resultado) <- c("Mes", "Severidad 1", "Severidad 2", "Severidad 3", "Severidad 4")

# Calculo de las cantidades de accidentes por mes y tipo de severidad
for (i in 1:12) {
  mes <- sprintf("%02d", i)
  tabla_resultado[i, 1] <- mes
  for (j in 2:5) {
    severidad <- j - 1
    frecuencia <- sum(dataAccidents$Severity == severidad & dataAccidents$Month == mes)
    tabla_resultado[i, j] <- frecuencia
  }
}


print(tabla_resultado)
write.csv(tabla_resultado, "dataMonth.csv", row.names=FALSE)


######################################################################################################################################

#accidentes por meses4
dataAccidents$Month <- format(as.POSIXct(dataAccidents$Start_Time), format = "%m")

tabla_resultado <- matrix(0, nrow = 12, ncol = 2)
colnames(tabla_resultado) <- c("Mes", "Total Accidentes")

# Calculo del total de accidentes por mes
for (i in 1:12) {
  mes <- sprintf("%02d", i)
  tabla_resultado[i, 1] <- mes
  frecuencia <- sum(dataAccidents$Month == mes & format(as.POSIXct(dataAccidents$Start_Time), format = "%Y") %in% c("2016", "2017", "2018", "2019", "2020", "2021", "2022"))
  tabla_resultado[i, 2] <- frecuencia
}
write.csv(tabla_resultado, "accidentesMeses.csv", row.names=FALSE) 

#print(tabla_resultado)


#######################################################################################################################################



#accidentes por mes y condicion del tiempo 5

dataAccidents$Month <- format(as.POSIXct(dataAccidents$Start_Time), format = "%m")

# las condiciones de tiempo 
condiciones_tiempo <- c("Rain", "Snow", "Thunderstorm", "Clear", "Overcast", "Mostly Cloudy", "Partly Cloudy", "Scattered Clouds")

# Crear una tabla vacía para almacenar los resultados
tabla_resultado <- matrix(0, nrow = 12, ncol = length(condiciones_tiempo) + 2)
colnames(tabla_resultado) <- c("Mes", condiciones_tiempo, "Otro")

# Calcular las frecuencias de accidentes por mes y condición del tiempo
for (i in 1:12) {
  mes <- sprintf("%02d", i)
  tabla_resultado[i, 1] <- mes
  for (j in 2:(length(condiciones_tiempo) + 1)) {
    condicion <- condiciones_tiempo[j - 1]
    frecuencia <- sum(dataAccidents$Month == mes & dataAccidents$Weather_Condition == condicion)
    tabla_resultado[i, j] <- frecuencia
  }
  # Calcular la frecuencia para otros tipos de condición del tiempo
  otros <- sum(dataAccidents$Month == mes & !(dataAccidents$Weather_Condition %in% condiciones_tiempo))
  tabla_resultado[i, length(condiciones_tiempo) + 2] <- otros
}

write.csv(tabla_resultado, "dataMonthAndWeatherCondition.csv", row.names=FALSE)


###############################################################################

# 6
Date <- as.POSIXct(dataAccidents$Start_Time, format="%Y-%m-%d %H:%M:%S")

diaHora <- as.data.frame.matrix(table(lubridate::wday(Date, label=TRUE, abbr=FALSE), format(Date, format="%H")))

datos_ancho <- pivot_wider(diaHora, names_from = DiaSemana, values_from = c(Hora00:Hora23))


write.csv(diaHora, "horaAccidente.csv", row.names=FALSE) #save CSV

###############################################################################

# 7 Mapa Estados y ciudades

dataAccidents$State <- as.factor(dataAccidents$State)
levels(dataAccidents$State)



estados <- as.data.frame(table(dataAccidents$State))
write.csv(estados, "estados.csv", row.names=FALSE)


cities <- as.data.frame(table(dataAccidents$City))
topCities <- tail(cities[ with(cities, order(Freq)),],100)

for(c in topCities$Var1){
  topCities$lon[topCities$Var1==c] <- mean(dataAccidents$Start_Lng[dataAccidents$City==c])
  topCities$lat[topCities$Var1==c] <- mean(dataAccidents$Start_Lat[dataAccidents$City==c])
  
}

write.csv(topCities, "topCiudades.csv", row.names=FALSE)

###############################################################################


# 8

diaHora <- as.data.frame.matrix(table(lubridate::wday(Date, label=TRUE, abbr=FALSE),format(Date, format="%H")))


write.csv(diaHora, "diaHora.csv", row.names=TRUE) 




