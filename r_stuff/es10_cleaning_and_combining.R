# IMPORT DEI DATAFRAME DI BASE
sales = read.csv2("sales.csv", stringsAsFactors = T)
View(sales)

observations = read.csv2("observations.csv", stringsAsFactors = T)
View(observations)

# ELIMINAZIONE COLONNE INDESIDERATE

# Colonne di NA
sales = sales[which(colnames(sales) != "X")]
sales = sales[which(colnames(sales) != "X.1")]
summary(sales)

# Colonne ripetute
sales = sales[which(colnames(sales) != "name_of_country.1")]
sales = sales[which(colnames(sales) != "year.1")]
summary(sales)

# Dati in countryname convertiti in uppercase
observations$countryname = toupper(observations$countryname)
summary(observations)

# COMBINING CON MERGE
so = merge(sales, observations, by.x = c("name_of_country", "year"), by.y = c("countryname", "year"), no.dups = T)
View(so)
summary(so)

# RINOMINAZIONE COLONNE
colnames(so) = c("Country", "Year", "Bikes", "Total_turnover", "Pop")
summary(so)

# ELIMINAZIONE RIGHE CON NA
so = na.omit(so)
na.omit(so, cols = c("Country", "Year"))
