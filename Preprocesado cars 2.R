library(tidyverse)
library(sjmisc)
cars_raw <- read.csv(file="./datos/db_merged_expanded_2.csv",
                     sep=";")
cars <- cars_raw

### We fix text patterns in variables

# cars <- cars |>
#   mutate(across(
#     where(is.character),
#     ~ str_remove_all(.x, "^.*\\(| hp\\)")
#   ))

cars$Power <- str_remove_all(cars$Power, "^.*\\(| hp\\)")
cars$price <- str_remove_all(cars$price, "€ ")
cars$Cash.price <- str_remove_all(cars$Cash.price, "€ |,-")
cars$Down.payment <- str_remove_all(cars$Down.payment, "€ |,-")
cars$Net.loan.amount <- str_remove_all(cars$Net.loan.amount, "€ |,-")
cars$Gross.loan.amount <- str_remove_all(cars$Gross.loan.amount, "€ |,-")
cars$Processing.fees <- str_remove_all(cars$Processing.fees, "€ |,-")
cars$Final.installment <- str_remove_all(cars$Final.installment, "€ |,-")
cars$Monthly.rate <- str_remove_all(cars$Monthly.rate, "€ |,-")
cars$Mileage <- str_remove_all(cars$Mileage, " km")
cars$Electric.Range..EAER. <- str_remove_all(cars$Electric.Range..EAER., " km")
cars$Electric.Range <- str_remove_all(cars$Electric.Range, " km")
cars$Electric.Range.7 <- str_remove_all(cars$Electric.Range.7, " km")
cars$Electric.Range..EAER..7 <- str_remove_all(cars$Electric.Range..EAER..7, " km")
cars$Engine.size <- str_remove_all(cars$Engine.size, " cc")
cars$Empty.weight <- str_remove_all(cars$Empty.weight, " kg")
cars$CO..emissions <- str_remove_all(cars$CO..emissions, fixed(" g/km (comb.)"))
cars$CO..emissions <- str_remove_all(cars$CO..emissions, fixed(" g/km (comb)"))
cars$Fuel.consumption <- str_remove_all(cars$Fuel.consumption, fixed(" l/100 km (comb.)"))
cars$Fuel.consumption <- str_remove_all(cars$Fuel.consumption, fixed(" kg/100 km (comb.)"))
cars$Power.consumption <- str_remove_all(cars$Power.consumption, fixed(" kWh/100 km (comb.)"))
cars$Charging.time.from.10..to.80. <- str_remove_all(cars$Charging.time.from.10..to.80., " min")
cars$Term <- str_remove_all(cars$Term, " Months")
cars$Effective.annual.interest <- str_remove_all(cars$Effective.annual.interest, " %")
cars$Effective.annual.interest <- str_replace_all(cars$Effective.annual.interest, ",", ".")
cars$Fixed.borrowing.rate.p.a. <- str_remove_all(cars$Fixed.borrowing.rate.p.a., " %")
cars$Fixed.borrowing.rate.p.a. <- str_replace_all(cars$Fixed.borrowing.rate.p.a., ",", ".")
cars$Availability <- str_remove_all(cars$Availability, "in | days after order| day after order")


cols <- c(
  "Cash.price", "Down.payment", "Net.loan.amount", "Gross.loan.amount",
  "Processing.fees", "Final.installment", "Monthly.rate"
)

# apply the swap to each column in-place
cars[cols] <- lapply(cars[cols], function(x) {
  # make sure it’s character
  x <- as.character(x)
  # step A: dots → placeholder
  x <- gsub("\\.", "__DOT__", x)
  # step B: commas → dots
  x <- gsub(",", ".", x, fixed = TRUE)
  # step C: placeholder → commas
  gsub("__DOT__", ",", x, fixed = TRUE)
})


cars <-
  cars |> select(-c(comfort_convenience_nan, entertainment_media_nan,
                    safety_security_nan, extras_nan,
                    comfort_convenience_1, comfort_convenience_2, comfort_convenience_3, comfort_convenience_4, comfort_convenience_5, comfort_convenience_6, comfort_convenience_7, comfort_convenience_8, comfort_convenience_9, comfort_convenience_10, comfort_convenience_11, comfort_convenience_12, comfort_convenience_13, comfort_convenience_14, comfort_convenience_15, comfort_convenience_16, comfort_convenience_17, comfort_convenience_18, comfort_convenience_19, comfort_convenience_20, comfort_convenience_21, comfort_convenience_22, comfort_convenience_23, comfort_convenience_24, comfort_convenience_25, comfort_convenience_26, comfort_convenience_27, comfort_convenience_28, comfort_convenience_29, comfort_convenience_30, comfort_convenience_31, comfort_convenience_32, comfort_convenience_33, comfort_convenience_34, comfort_convenience_35, comfort_convenience_36, comfort_convenience_37, comfort_convenience_38,
                    entertainment_media_1, entertainment_media_2, entertainment_media_3, entertainment_media_4, entertainment_media_5, entertainment_media_6, entertainment_media_7, entertainment_media_8, entertainment_media_9, entertainment_media_10, entertainment_media_11, entertainment_media_12, entertainment_media_13, entertainment_media_14, entertainment_media_15, entertainment_media_16,
                    safety_security_1, safety_security_2, safety_security_3, safety_security_4, safety_security_5, safety_security_6, safety_security_7, safety_security_8, safety_security_9, safety_security_10, safety_security_11, safety_security_12, safety_security_13, safety_security_14, safety_security_15, safety_security_16, safety_security_17, safety_security_18, safety_security_19, safety_security_20, safety_security_21, safety_security_22, safety_security_23, safety_security_24, safety_security_25, safety_security_26, safety_security_27, safety_security_28, safety_security_29, safety_security_30, safety_security_31, safety_security_32, safety_security_33, safety_security_34,
                    extras_1, extras_2, extras_3, extras_4, extras_5, extras_6, extras_7, extras_8, extras_9, extras_10, extras_11, extras_12, extras_13, extras_14, extras_15, extras_16, extras_17, extras_18, extras_19, extras_20, extras_21, extras_22))

cars[cars == ""] <- NA

cars$price <- str_remove_all(cars$price, ",")
cars$Mileage <- str_remove_all(cars$Mileage, ",")
cars$Power <- str_remove_all(cars$Power, ",")
cars$Engine.size <- str_remove_all(cars$Engine.size, ",")
cars$Empty.weight <- str_remove_all(cars$Empty.weight, ",")
cars$CO..emissions <- str_remove_all(cars$CO..emissions, ",")

cars <-
  cars |>
  mutate(across(c(price, Mileage, Power, Engine.size,
                  Empty.weight, CO..emissions),
                as.double))

cars$Seats[cars$Seats == max(cars$Seats, na.rm = TRUE)] <-
  median(cars$Seats, na.rm = TRUE)

cars <- cars %>%
  group_by(car_make, car_model) %>%
  mutate(
    drivetrain_mode = first(Drivetrain[!is.na(Drivetrain)]),
    Drivetrain = if_else(
      is.na(Drivetrain),
      drivetrain_mode,
      Drivetrain
    )
  ) %>%
  select(-drivetrain_mode) %>%
  ungroup()

cars <- cars %>%
  group_by(car_make, car_model) %>%
  mutate(
    emptyweight_mode = first(Empty.weight[!is.na(Empty.weight)]),
    Empty.weight = if_else(
      is.na(Empty.weight),
      emptyweight_mode,
      Empty.weight
    )
  ) %>%
  select(-emptyweight_mode) %>%
  ungroup()

# Drop NAs in car_model, Drivetrain, Gearbox, Seats, Doors, Power, Empty.weight
# If Type == "New", Mileage = 0, Production.date = 2025, First.registration = "05/2025", Previous.owner = 0
# If Fuel.type == "Electric", Engine.size = 0
# If Fuel.type != "Electric/Gasoline", "Electric/Diesel" and "Electric", Electric.Range = 0, Electric.Range..EAER. = 0, Electric.Range.7 = 0, Electric.Range..EAER..7 = 0, Charging.time.from.10..to.80. = 0
# Eliminate Gears, Cylinders, Available.from, Cash.price, Down.payment, Term, Net.loan.amount, Gross.loan.amount, Processing.fees, Effective.annual.interest, Fixed.borrowing.rate.p.a., Final.installment, Last.service, Power.consumption, Country.version, Offer.number, Model.code
# Convert to binary Warranty, CO..efficiency, General.inspection, Full.service.history, Non.smoker.vehicle, Battery.certificate, Monthly.rate
# Create None category in Other.fuel.types
# Create Unknown category in Energy.efficiency.class, Availability, Battery.Ownership, Emission.class, Emissions.sticker, Manufacturer.colour, Paint, Upholstery.colour, Upholstery

cars <- cars |> 
  select(-c(Gears, Cylinders, Available.from, Cash.price, Down.payment, Term,
            Net.loan.amount, Gross.loan.amount, Processing.fees,
            Effective.annual.interest, Fixed.borrowing.rate.p.a.,
            Final.installment, Last.service, Power.consumption,
            Country.version, Offer.number, Model.code, X))

cars$Engine.size[cars$Fuel.type == "Electric"] <- 0

range_cols <- c(
  "Electric.Range",
  "Electric.Range..EAER.",
  "Electric.Range.7",
  "Electric.Range..EAER..7",
  "Charging.time.from.10..to.80."
)

cars <- cars %>%
  mutate(
    across(
      all_of(range_cols),
      ~ if_else(
        !(Fuel.type %in% c("Electric", "Electric/Gasoline", "Electric/Diesel")),
        "0",     # character zero
        .x       # original character values
      )
    )
  ) %>%
  # now convert all of them to numeric
  mutate(across(all_of(range_cols), as.numeric))

cars <- cars %>%
  mutate(
    Mileage                 = if_else(Type == "New", 0, Mileage),
    Production.date         = if_else(Type == "New", 2025, Production.date),
    First.registration      = if_else(Type == "New", "05/2025", First.registration),
    Previous.owner          = if_else(Type == "New", 0, Previous.owner),
    Other.fuel.types        = if_else(is.na(Other.fuel.types), "None", Other.fuel.types),
    Upholstery              = if_else(is.na(Upholstery), "Unknown", Upholstery),
    Upholstery.colour       = if_else(is.na(Upholstery.colour), "Unknown", Upholstery.colour),
    Paint                   = if_else(is.na(Paint), "Unknown", Paint),
    Manufacturer.colour     = if_else(is.na(Manufacturer.colour), "Unknown", Manufacturer.colour),
    Colour                  = if_else(is.na(Colour), "Unknown", Colour),
    Emissions.sticker       = if_else(is.na(Emissions.sticker), "Unknown", Emissions.sticker),
    Emission.class          = if_else(is.na(Emission.class), "Unknown", Emission.class),
    Battery.Ownership       = if_else(is.na(Battery.Ownership), "Unknown", Battery.Ownership),
    Availability            = if_else(is.na(Availability), "Unknown", Availability),
    Energy.efficiency.class = if_else(is.na(Energy.efficiency.class), "Unknown", Energy.efficiency.class),
    Monthly.rate            = if_else(is.na(Monthly.rate), 0, 1),
    Battery.certificate     = if_else(is.na(Battery.certificate), 0, 1),
    Non.smoker.vehicle      = if_else(is.na(Non.smoker.vehicle), 0, 1),
    Full.service.history    = if_else(is.na(Full.service.history), 0, 1),
    General.inspection      = if_else(is.na(General.inspection), 0, 1),
    CO..efficiency          = if_else(is.na(CO..efficiency), 0, 1),
    Fuel.consumption        = if_else(is.na(Fuel.consumption), 0, 1),
    Warranty                = if_else(is.na(Warranty) | Warranty == "0 months", 0, 1)
  )

library(lubridate)

cutoff <- ymd("2025-05-01")

cars <- cars %>%
  mutate(
    reg_date        = my(First.registration),              # e.g. "05/2023" → "2023-05-01"
    days_since_reg  = as.integer(cutoff - reg_date)
  )

cars <- cars |> select(-c(First.registration, reg_date))

library(skimr)
skim(cars)




colnames(cars)[colnames(cars) == 'Monthly.rate'] <- 'Finance'

cars <- cars |> relocate(days_since_reg, .after = Electric.Range..EAER..7)

cars <- cars |> 
  mutate(
    car_name = paste(car_make, car_model, sep = " ")
  )

cars <- cars |> relocate(car_name, .before = car_make)
cars <- cars |> relocate(X_src, .before = car_name)

cars <- cars |> select(-Production.date)

cars$Previous.owner[is.na(cars$Previous.owner)] <- 1

cars <- cars |>
  slice(-order(Previous.owner, decreasing = TRUE)[1:2])

cars <- cars |> 
  mutate(Fuel.type =
           case_when(grepl("(Diesel|Diesel (Particle filter)|
                           Diesel (Particle filter) / Biodiesel / Vegetable oil)",
                           Fuel.type, ignore.case = TRUE) ~ "Diesel",
                     Fuel.type == "CNG" ~ "CNG",
                     Fuel.type == "Electric" ~ "Electric",
                     Fuel.type == "Electric/Diesel" ~ "Electric/Diesel",
                     Fuel.type == "Electric/Gasoline" ~ "Electric/Gasoline",
                     Fuel.type == "Gasoline" ~ "Gasoline",
                     Fuel.type == "LPG" ~ "LPG",
                     TRUE ~ "Others"))

cars <- cars |>
  slice(-order(CO..emissions, decreasing = TRUE)[1:2])

cars <- cars |>
  slice(-order(price, decreasing = TRUE)[1:2])

cars <- cars |>
  slice(-order(Mileage, decreasing = TRUE)[1])

cars <- cars |>
  slice(-order(Empty.weight, decreasing = TRUE)[1])

cars <- cars |>
  slice(-order(Empty.weight, decreasing = FALSE)[1])

cars <- cars |> 
  mutate(CO..emissions = ifelse(
    is.na(CO..emissions), median(CO..emissions, na.rm = TRUE), CO..emissions))


cars <- cars |> select(-X_src)

tabla <- cars |>
  group_by(car_make) |>
  filter(Type == "New") |> 
  summarise(precio_medio = mean(price)) |>
  mutate(category =
           case_when(precio_medio <= 19900 ~ "low_cost",
                     precio_medio <= 28990 ~ "standard",
                     precio_medio <= 49900 ~ "upper_standard",
                     precio_medio > 49900 ~ "premium")) |> 
  ungroup()

lookup_table <- setNames(tabla$category, tabla$car_make)
cars$car_make_category <- lookup_table[cars$car_make]

cars <- cars |> relocate(car_make_category, .before = price)

cars <- cars |> 
  mutate(car_make_category =
           ifelse(is.na(car_make_category),
                  ifelse(price<=19900, "low_cost",
                         ifelse(price>19900&price<=28990, "standard",
                                ifelse(price>28990&price<=49900, "upper_standard",
                                       "premium"))), car_make_category))

cars |> group_by(Availability) |> summarise(mean = mean(price)) |> ungroup()

cars |>
  count(Availability, sort = TRUE) |> 
  mutate(porc = 100*n/sum(n), cumul = cumsum(porc))

cars <- cars |> 
  mutate(Availability =
           case_when(Availability == "120" ~ "120",
                     Availability == "Unknown" ~ "Unknown",
                     grepl("(180|150|270|360)",
                           Availability, ignore.case = FALSE) ~ ">120",
                     grepl("(28|90|60|1|3|7|42|14|2|5|4|6|21)",
                           Availability, ignore.case = FALSE) ~ "<120"))



### We finish the preprocessing ###

skim(cars)

cars <- cars |> filter(Fuel.type == "Electric/Gasoline" |
                         Fuel.type == "Electric")

skim(cars)

cars <- cars |> 
  select(-c(Electric.Range..EAER., Electric.Range,
            Charging.time.from.10..to.80., Electric.Range.7,
            Electric.Range..EAER..7))

cars <- cars %>%
  drop_na(car_model, Drivetrain, Gearbox, Seats, Doors, Power,
          Engine.size, Empty.weight)

cars |> group_by(Fuel.type) |> count() |> ungroup()


# We save the dataframe

library(openxlsx)

# Specify the file path where you want to save the Excel file
file_path <- "./datos/db_merged_expanded_2_EV.xlsx"

# Write the dataframe to an Excel file
write.xlsx(cars, file_path, sheetName = "Sheet1", rowNames = FALSE)
write_csv(cars, "./datos/db_merged_expanded_2_EV.csv")



# We open the file again

cars <- read.csv(file="./datos/db_merged_expanded_2_EV.csv",
                 sep=",")

range_table <- read.xlsx(xlsxFile="./datos/range_fixed.xlsx")
range_table <- range_table |> 
  select(-source)


cars_enriched <- cars %>%
  left_join(
    range_table %>% 
      select(
        car_name,
        vehicle_type,
        range,
        charging_time
      ),
    by = "car_name"
  )


cars_enriched |> group_by(Fuel.type) |>
  count(vehicle_type) |> ungroup()


cars_enriched <- cars_enriched |> 
  filter(vehicle_type != "ICE")

cars_enriched <- cars_enriched |> 
  filter(!(
    # 1) Electric but typed as MHEV or PHEV
    (Fuel.type == "Electric" & vehicle_type %in% c("MHEV", "PHEV"))
    |
      # 2) Electric/Gasoline but typed as BEV
      (Fuel.type == "Electric/Gasoline" & vehicle_type == "BEV")
  ))


cars <- cars_enriched
rm(range_table)




#### Random forest to select the best variables
cars <- cars |> mutate(across(where(is.character), as_factor))

### Variable selection in regression with price as target

# Normalización de variables discretas y continuas
var_cuali <- cars |> select(where(is.factor)) |> colnames()
var_num <- cars |> 
  select(-price) |> 
  select_if(function(x) is.numeric(x) && !all(x %in% c(0, 1))) |> 
  colnames()
var_bin <- cars |> 
  select_if(function(x) is.numeric(x) && all(x %in% c(0, 1))) |> 
  colnames()
vardep <- "price"

# Función de normalización Min-Max
min_max <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

cars2 <- as.data.frame(lapply(cars[,var_num], min_max))
cars<-data.frame(cbind(cars2,cars[,c(var_bin,var_cuali,vardep)]))

for(i in var_cuali){print(cars[[i]] |> n_distinct())}

# We eliminate car_name, car_model, Manufacturer.colour, dealer and car_location

cars <- cars |> select(-c(car_name, car_make, car_model, Manufacturer.colour,
                          dealer, car_location))

library(caret)
library(dummies)
source("funcion steprepetido.R")

cars2 <- dummy.data.frame(cars, var_cuali, sep = ".")
nombres1 <- cars2 |> 
  select(-price) |> 
  colnames()
vardep <- "price"


lista<-steprepetido(data=cars2,vardep=vardep,
                    listconti=nombres1,
                    sinicio=12345,sfinal=12385,
                    porcen=0.8,criterio="BIC")

tabla<-lista[[1]]
dput(lista[[2]][[1]])
# c("Power", "Body.type.Coupe", "Body.type.Convertible", "Mileage", 
#   "Engine.size", "X.Fuel.type.Electric.Gasoline.", "CO..emissions", 
#   "comfort_convenience_Automatic.climate.control..4.zones", "Drivetrain.Front", 
#   "X.Emission.class.Euro.6c.", "days_since_reg", "Seats", "Body.type.Van", 
#   "comfort_convenience_Massage.seats", "X.Emission.class.Euro.5.", 
#   "Non.smoker.vehicle", "car_make_category.upper_standard", "Emissions.sticker.Unknown", 
#   "Empty.weight", "X.Body.type.Off.Road.Pick.up.", "vehicle_type.MHEV", 
#   "Availability.Unknown", "entertainment_media_Television", "comfort_convenience_Wind.deflector"
# )

dput(lista[[2]][[2]])
# c("Power", "Body.type.Coupe", "Body.type.Convertible", "Mileage", 
#   "Engine.size", "X.Fuel.type.Electric.Gasoline.", "CO..emissions", 
#   "Drivetrain.Front", "Seats", "Body.type.Van", "X.Emission.class.Euro.6c.", 
#   "comfort_convenience_Automatic.climate.control..4.zones", "days_since_reg", 
#   "car_make_category.upper_standard", "X.Emission.class.Euro.5.", 
#   "Emissions.sticker.Unknown", "comfort_convenience_Massage.seats", 
#   "Non.smoker.vehicle", "entertainment_media_CD.player", "entertainment_media_Television", 
#   "comfort_convenience_Wind.deflector", "Empty.weight", "X.Body.type.Off.Road.Pick.up.", 
#   "vehicle_type.MHEV", "Availability.Unknown")


### Random forest

library(randomForest)

rfgrid<-expand.grid(mtry=c(5,27,75))

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

rf<- caret::train(price~.,data=cars2,
                  method="rf",trControl=control,tuneGrid=rfgrid,
                  linOut = TRUE,ntree=300,nodesize=10,replace=TRUE,
                  importance=TRUE)
rf


### CON IMPORTANCIA DE VARIABLES RANDOM FOREST

final<-rf$finalModel
tabla<-as.data.frame(importance(final))
tabla<-tabla[order(-tabla$IncNodePurity),]
tabla


#                                                             %IncMSE  IncNodePurity
# Power                                                   19.85368200  5.194001e+12
# Engine.size                                             14.53071302  3.143682e+12
# Doors                                                    9.39870867  9.045934e+11
# Seats                                                    8.60462022  5.995396e+11
# CO..emissions                                            9.65196756  5.972075e+11
# Empty.weight                                            11.37388574  5.832845e+11
# Body.type.Coupe                                          5.42897239  4.866760e+11
# Body.type.Convertible                                    4.56979281  2.289030e+11
# car_make_category.premium                                6.14648094  2.101547e+11
# range                                                    9.00498964  2.062971e+11
# Drivetrain.4WD                                           7.70304120  1.872208e+11
# Mileage                                                 11.63170651  1.771550e+11
# days_since_reg                                          15.57014975  1.712638e+11
# vehicle_type.PHEV                                        4.83784078  1.337661e+11
# charging_time                                            5.09870832  8.982847e+10
# Fuel.type.Electric                                       3.18219676  5.649759e+10
# vehicle_type.BEV                                         3.05981916  5.615985e+10
# Drivetrain.Front                                         4.68758284  5.589056e+10
# ratings_number                                           4.50989422  5.434881e+10
# `Fuel.type.Electric/Gasoline`                            3.29513638  4.047694e+10
# Drivetrain.Rear                                          4.69893691  3.713278e+10
# Previous.owner                                           7.35954303  2.828146e+10
# Body.type.Sedan                                          5.02630450  2.511263e+10
# `Upholstery.Full leather`                                2.98975301  2.429827e+10
# Type.New                                                 9.69472877  2.243970e+10
# Upholstery.Cloth                                         4.53378651  2.242303e+10
# Type.Used                                                4.26806415  2.045938e+10
# `Body.type.Off-Road/Pick-up`                             5.28688179  1.475698e+10
# Paint.Unknown                                           -1.04192199  1.392035e+10
# Emissions.sticker.Unknown                                0.06345107  1.336054e+10
# Non.smoker.vehicle                                       2.73207143  1.231132e+10
# `Emissions.sticker.4 (Green)`                            1.32691567  1.225931e+10
# Warranty                                                 0.72598390  1.127116e+10
# Paint.Others                                             2.93875218  1.092710e+10
# car_make_category.standard                               1.45681911  1.070843e+10
# car_make_category.upper_standard                         5.70412251  8.507111e+09
# General.inspection                                       2.81356616  8.199166e+09
# `Emission.class.Euro 6d`                                 2.50190946  8.159061e+09
# comfort_convenience_Automatic.climate.control            1.50131616  7.391861e+09
# Upholstery.colour.Unknown                                3.84484777  7.310560e+09
# Colour.Red                                              -0.19547619  7.286369e+09
# Upholstery.colour.Other                                  2.53802052  7.247090e+09
# Full.service.history                                     1.54129192  7.214808e+09
# `Upholstery.Part leather`                                2.07795022  7.121755e+09
# `Emission.class.Euro 6d-TEMP`                            1.72068661  7.045034e+09
# Paint.Metallic                                          -0.44744408  6.568401e+09
# `Emission.class.Euro 6`                                  0.55044680  6.528263e+09
# Emission.class.Unknown                                   1.79404580  6.191831e+09
# Colour.White                                             1.09892406  6.153743e+09
# Upholstery.colour.Beige                                  1.21218965  6.030412e+09
# Colour.Black                                             0.91414066  5.943357e+09
# `Emission.class.Euro 5`                                  3.05862785  5.897117e+09
# Upholstery.alcantara                                     1.34620767  5.597859e+09
# comfort_convenience_Air.suspension                       0.43640567  5.542178e+09
# `Body.type.Station wagon`                                3.62692036  5.240508e+09
# Body.type.Van                                            8.87097851  5.174265e+09
# `Emission.class.Euro 6c`                                 2.19209511  4.858344e+09
# Upholstery.colour.Black                                  1.61366096  4.717601e+09
# car_make_category.low_cost                               1.60775390  4.490783e+09
# Body.type.Compact                                        9.16445795  4.455478e+09
# Colour.Green                                             1.17248742  4.443316e+09
# Upholstery.colour.Blue                                  -1.01332954  3.765322e+09
# Colour.Yellow                                           -0.43914126  3.712143e+09
# extras_Voice.Control                                     2.48260598  3.531028e+09
# vehicle_type.HEV                                         3.29403862  3.476568e+09
# comfort_convenience_Parking.assist.system.camera         1.90536252  3.307866e+09
# safety_security_Central.door.lock.with.remote.control    1.63111041  3.196899e+09
# entertainment_media_elements                            -0.88275550  3.071652e+09
# Colour.Grey                                              1.02621487  3.035378e+09
# Upholstery.colour.Brown                                  0.78910081  3.007291e+09
# extras_Sport.package                                     0.40526624  2.657172e+09
# extras_Catalytic.Converter                               1.90447509  2.647424e+09
# safety_security_Full.LED.headlights                      1.51231644  2.620984e+09
# Upholstery.Unknown                                       0.87735143  2.570452e+09
# safety_security_elements                                 1.08095318  2.353699e+09
# safety_security_Fog.lights                              -0.52097687  2.348225e+09
# Colour.Blue                                              0.91298585  2.240455e+09
# Upholstery.colour.Red                                    1.44714118  2.155786e+09
# comfort_convenience_elements                             2.20636746  2.103035e+09
# safety_security_Passenger.side.airbag                   -0.95782643  2.064461e+09
# comfort_convenience_Air.conditioning                     0.62850757  1.749874e+09
# comfort_convenience_Parking.assist.system.sensors.front  1.00141469  1.641375e+09
# Colour.Orange                                            0.46345892  1.630164e+09
# Colour.Silver                                           -0.46924211  1.568737e+09
# extras_Sport.suspension                                 -1.01956088  1.486405e+09
# extras_elements                                          1.17014711  1.451466e+09
# entertainment_media_WLAN...WiFi.hotspot                 -1.36271954  1.418913e+09
# extras_Summer.tyres                                     -0.55837734  1.380914e+09
# Upholstery.colour.Grey                                   2.01948248  1.351524e+09
# Availability.Unknown                                     4.00000094  1.286406e+09
# comfort_convenience_Heads.up.display                     1.03383790  1.265167e+09
# safety_security_LED.Daytime.Running.Lights              -1.05186763  1.261325e+09
# Upholstery.Other                                         2.16882969  1.245960e+09
# entertainment_media_MP3                                  1.03043274  1.240792e+09
# safety_security_LED.Headlights                          -1.52321115  1.178937e+09
# comfort_convenience_Electrical.side.mirrors             -0.09015806  1.103421e+09
# comfort_convenience_Park.Distance.Control                1.30406943  1.090386e+09
# comfort_convenience_Cruise.control                       1.36395917  1.042610e+09
# entertainment_media_Integrated.music.streaming           1.42480917  1.026211e+09
# comfort_convenience_Sunroof                              0.06784463  9.732792e+08
# vehicle_type.MHEV                                        3.76674522  9.425256e+08
# safety_security_Glare.free.high.beam.headlights          1.09670558  8.748011e+08
# `Emission.class.Euro 6e`                                 2.47519053  8.677144e+08
# comfort_convenience_Leather.seats                       -0.62080530  8.629563e+08
# extras_Ambient.lighting                                  1.53660037  8.488128e+08
# entertainment_media_Apple.CarPlay                       -0.26079490  8.253246e+08
# Battery.Ownership.Included                               2.83699099  7.715760e+08
# entertainment_media_Television                           0.00000000  7.710393e+08
# extras_Emergency.tyre.repair.kit                         1.21307417  7.663477e+08
# comfort_convenience_Panorama.roof                       -0.63098436  7.638708e+08
# safety_security_Emergency.system                         0.68277157  7.541135e+08
# entertainment_media_Radio                               -0.90387647  7.470854e+08
# safety_security_Electronic.stability.control            -0.06230908  7.427941e+08
# safety_security_Driver.drowsiness.detection             -1.40588254  7.001007e+08
# `Availability.<120`                                      2.23942874  6.957194e+08
# Battery.Ownership.Unknown                                2.26910161  6.700542e+08
# safety_security_Distance.warning.system                  1.02762132  6.625179e+08
# safety_security_Side.airbag                              1.29115925  6.373178e+08
# entertainment_media_CD.player                           -1.69475128  6.345022e+08
# extras_Particle.filter                                   0.01306392  6.343255e+08
# entertainment_media_Hands.free.equipment                 1.19747922  6.183924e+08
# comfort_convenience_Massage.seats                        2.28889459  6.011477e+08
# comfort_convenience_Parking.assist.system.sensors.rear   0.62169842  6.005415e+08
# extras_Electronic.parking.brake                         -0.99190301  5.983292e+08
# Finance                                                  2.14093237  5.910918e+08
# safety_security_Alarm.system                             0.97661873  5.904231e+08
# extras_Trailer.hitch                                     4.22221737  5.578796e+08
# extras_Sport.seats                                      -1.78697930  5.556385e+08
# `Emissions.sticker.1 (No sticker)`                       0.39288141  5.417356e+08
# Colour.Violet                                            2.21221131  4.926973e+08
# comfort_convenience_Armrest                              2.00923481  4.850583e+08
# extras_None                                              0.53316067  4.676117e+08
# comfort_convenience_Automatic.climate.control..4.zones   0.85083575  4.565224e+08
# comfort_convenience_Keyless.central.door.lock            0.95354667  4.514321e+08
# extras_Headlight.washer.system                          -0.73117287  4.197701e+08
# comfort_convenience_Parking.assist.system.self.steering -1.08247381  4.052460e+08
# extras_E10.enabled                                      -0.96798021  4.024819e+08
# `Availability.>120`                                      2.96000066  3.954758e+08
# safety_security_Traction.control                        -0.54100445  3.876528e+08
# safety_security_Adaptive.Cruise.Control                  1.19382851  3.655019e+08
# Battery.certificate                                      1.29258668  3.648715e+08
# safety_security_Lane.departure.warning.system           -0.76717555  3.491682e+08
# extras_Shift.paddles                                    -0.68977324  3.214620e+08
# extras_Alloy.wheels..20..                                2.17827501  3.209177e+08
# safety_security_Traffic.sign.recognition                 1.27121046  3.175316e+08
# comfort_convenience_Tinted.windows                       0.28614818  3.033227e+08
# Availability.120                                        -0.58286429  3.012283e+08
# entertainment_media_None                                 0.71002665  3.011708e+08
# entertainment_media_USB                                  1.47319241  2.846678e+08
# comfort_convenience_Electrically.adjustable.seats        0.98015303  2.795179e+08
# extras_Spoiler                                           1.62263243  2.616308e+08
# Colour.Unknown                                           1.46597377  2.605321e+08
# comfort_convenience_Heated.steering.wheel                2.31911019  2.548214e+08
# comfort_convenience_Electric.tailgate                    3.23027495  2.401591e+08
# comfort_convenience_Seat.ventilation                     1.40361182  2.367908e+08
# comfort_convenience_Split.rear.seats                    -0.82534521  2.308447e+08
# Gearbox.Automatic                                        5.72397321  2.283036e+08
# safety_security_High.beam.assist                        -0.11275135  2.275149e+08
# safety_security_Central.door.lock                        1.12449419  2.200715e+08
# comfort_convenience_Power.windows                       -1.39843354  2.153767e+08
# safety_security_Laser.headlights                         0.90876942  2.079340e+08
# Gearbox.Manual                                           6.18372409  2.068557e+08
# Upholstery.Velour                                        2.50649873  2.013022e+08
# comfort_convenience_Hill.Holder                         -1.38638137  2.007378e+08
# extras_Spare.tyre                                        0.00000000  2.003166e+08
# comfort_convenience_Electric.backseat.adjustment        -1.00167084  1.953711e+08
# comfort_convenience_Start.stop.system                   -0.03075838  1.946825e+08
# comfort_convenience_Light.sensor                         0.37115299  1.917792e+08
# safety_security_Rear.airbag                             -0.47542648  1.792396e+08
# extras_Alloy.wheels                                      0.92082034  1.731116e+08
# comfort_convenience_Electrically.heated.windshield       1.75097509  1.678644e+08
# entertainment_media_Digital.cockpit                      1.40331662  1.640215e+08
# safety_security_Blind.spot.monitor                       1.44119124  1.635409e+08
# safety_security_Speed.limit.control.system               1.87832709  1.615317e+08
# comfort_convenience_Sliding.door.right                   2.62491157  1.571621e+08
# safety_security_Tire.pressure.monitoring.system         -0.66463554  1.549042e+08
# `Energy.efficiency.class.A+`                             2.72949890  1.526992e+08
# safety_security_Bi.Xenon.headlights                     -0.37572718  1.484172e+08
# extras_Ski.bag                                          -1.00495478  1.458714e+08
# extras_Touch.screen                                      1.38180877  1.399681e+08
# extras_Roof.rack                                         1.59824530  1.365480e+08
# entertainment_media_Digital.radio                        0.96937625  1.357808e+08
# safety_security_Head.airbag                              1.68695049  1.320692e+08
# comfort_convenience_Multi.function.steering.wheel        1.07538369  1.316353e+08
# safety_security_Immobilizer                              1.59726836  1.298109e+08
# entertainment_media_Induction.charging.for.smartphones   2.69450173  1.281222e+08
# Energy.efficiency.class.Unknown                          0.43209701  1.270611e+08
# Colour.Gold                                              1.18818062  1.257276e+08
# CO..efficiency                                           1.09320014  1.248618e+08
# entertainment_media_On.board.computer                    1.23387269  1.230374e+08
# safety_security_Daytime.running.lights                  -0.04226862  1.225367e+08
# comfort_convenience_Automatic.climate.control..3.zones  -0.77187986  1.126149e+08
# extras_Cargo.barrier                                     0.86300622  1.102919e+08
# safety_security_None                                     0.39862213  1.102815e+08
# comfort_convenience_Lumbar.support                       2.28243905  1.089967e+08
# comfort_convenience_Leather.steering.wheel               1.20874822  1.077386e+08
# safety_security_Emergency.brake.assistant                0.33081710  1.057367e+08
# comfort_convenience_Sliding.door.left                    1.24475034  9.693503e+07
# entertainment_media_Android.Auto                        -0.06572405  9.683514e+07
# comfort_convenience_360..camera                          2.70060341  9.176871e+07
# safety_security_Adaptive.headlights                      0.60125964  9.143178e+07
# comfort_convenience_Automatic.climate.control..2.zones   0.82856083  8.679447e+07
# safety_security_Power.steering                           0.98321786  8.068004e+07
# safety_security_Xenon.headlights                         1.08057752  7.809558e+07
# comfort_convenience_Rain.sensor                          1.59436775  7.250592e+07
# comfort_convenience_Auxiliary.heating                    1.33252359  7.163551e+07
# comfort_convenience_None                                -0.09626786  7.030347e+07
# comfort_convenience_Navigation.system                    0.91495406  6.324194e+07
# extras_Steel.wheels                                      2.17513626  5.435030e+07
# extras_Winter.package                                    0.97884185  5.420809e+07
# safety_security_Isofix                                  -1.31534449  5.222062e+07
# extras_Winter.tyres                                      0.00000000  5.158368e+07
# safety_security_Driver.side.airbag                       0.52910029  5.108200e+07
# entertainment_media_Sound.system                         2.21430732  5.084164e+07
# Other.fuel.types.None                                    1.39078733  4.755158e+07
# extras_Smoker.s.package                                  0.00000000  4.660924e+07
# safety_security_ABS                                      0.55784232  4.336242e+07
# extras_Automatically.dimming.interior.mirror            -0.86297123  3.993484e+07
# Fuel.consumption                                         0.82327771  3.934390e+07
# Other.fuel.types.Electricity                             0.81602541  3.880687e+07
# extras_Tuned.car                                         0.00000000  3.462213e+07
# comfort_convenience_Fold.flat.passenger.seat             1.00167084  2.878828e+07
# extras_Alloy.wheels..19..                                1.07944227  2.371018e+07
# comfort_convenience_Seat.heating                         2.28033975  2.345863e+07
# extras_Heat.pump                                         0.22456544  2.021927e+07
# `Energy.efficiency.class.A+++`                           0.07242333  1.905113e+07
# Upholstery.colour.White                                  1.00167084  1.828306e+07
# extras_Alloy.wheels..17..                                1.32426748  1.693032e+07
# Body.type.Other                                          1.09405250  1.569728e+07
# extras_Alloy.wheels..15..                                1.02467836  1.558609e+07
# comfort_convenience_Rear.seat.heating                    0.33352821  1.497751e+07
# extras_Alloy.wheels..18..                               -1.90501739  1.416456e+07
# `Emission.class.Euro 6b`                                 5.15481351  1.414770e+07
# entertainment_media_Bluetooth                            0.36439913  1.347296e+07
# extras_All.season.tyres                                 -1.37868936  8.054219e+06
# safety_security_Night.view.assist                        0.00000000  7.835706e+06
# Colour.Beige                                             0.90736225  6.908518e+06
# Body.type.Transporter                                    0.00000000  6.667825e+06
# Colour.Brown                                             1.12285055  4.152915e+06
# `Energy.efficiency.class.A++`                            1.49410360  3.940837e+06
# extras_Bidirectional.charging                            0.00000000  3.853921e+06
# Battery.Ownership.Rented                                -1.25674313  3.008046e+06
# extras_Alloy.wheels..21..                                0.00000000  2.997270e+06
# extras_Alloy.wheels..16..                                0.86297432  2.667415e+06
# extras_Emergency.tyre                                    1.00167084  1.754841e+06
# Colour.Bronze                                            0.00000000  1.302934e+06
# comfort_convenience_Wind.deflector                       0.00000000  6.769097e+05
# Energy.efficiency.class.E                                0.00000000  6.642724e+05
# Energy.efficiency.class.G                                0.00000000  6.044297e+05
# extras_Range.extender                                    0.00000000  4.607015e+05
# extras_Sliding.door                                      0.00000000  3.581313e+05
# `Gearbox.Semi-automatic`                                 0.00000000  2.650552e+05
# extras_Awning                                            0.00000000  2.597720e+05
# Energy.efficiency.class.A                                0.00000000  2.907339e+04
# extras_Alloy.wheels..12..                                0.00000000  0.000000e+00
# extras_Alloy.wheels..14..                                0.00000000  0.000000e+00
# extras_Alloy.wheels..22..                                0.00000000  0.000000e+00
# extras_Alloy.wheels..23..                                0.00000000  0.000000e+00
# extras_Differential.lock                                 0.00000000  0.000000e+00
# extras_Divider                                           0.00000000  0.000000e+00
# extras_Handicapped.enabled                               0.00000000  0.000000e+00
# extras_Municipal.vehicle                                 0.00000000  0.000000e+00
# extras_Right.hand.drive                                  0.00000000  0.000000e+00
# Upholstery.colour.Green                                  0.00000000  0.000000e+00
# Upholstery.colour.Orange                                 0.00000000  0.000000e+00
# Energy.efficiency.class.B                                0.00000000  0.000000e+00


### We select the following variables

cars <- cars_enriched

selected_vars <- c("price", "Power", "Engine.size", "Mileage", "Empty.weight",
                   "days_since_reg", "Seats", "CO..emissions",
                   "car_make_category", "Drivetrain", "ratings_number",
                   "Availability", "Doors", "Type", "Previous.owner",
                   "Body.type", "range", "charging_time",
                   "vehicle_type")

cars <- cars |> dplyr::select(all_of(selected_vars))

### We do the clustering process

library(factoextra)

var_cuali <- c("car_make_category", "Drivetrain", "Availability",
               "Type", "Fuel.type", "Body.type", "vehicle_type")

cars2 <- dummy.data.frame(cars, var_cuali, sep = ".")

fviz_nbclust(cars2, kmeans, method="wss")       # 3 clusters or 6
fviz_nbclust(cars2, kmeans, method="silhouette") # 2 clusters or 5


optimal_k <- 6   # for example, choose k = 6
set.seed(123)
km_final <- kmeans(cars2, centers = optimal_k, nstart = 25)
table(km_final$cluster)


# View cluster centers on the original (unscaled) variables:
cluster_profiles <- aggregate(cars, 
                              by = list(cluster = km_final$cluster), 
                              FUN = mean)
print(cluster_profiles)


# (You might pick, say, 5–7 features to highlight on a radar plot.)
library(fmsb)

# Example: select 6 variables of interest 
radar_vars <- c("price", "Engine.size", "Mileage", "Power",
                "Empty.weight", "ratings_number", "CO..emissions",
                "Drivetrain.Front", "Drivetrain.4WD",
                "vehicle_type.BEV", "vehicle_type.HEV",
                "vehicle_type.PHEV",
                "Body.type.Off-Road/Pick-up", "Body.type.Sedan",
                "Body.type.Coupe")
radar_data <- cluster_profiles[, radar_vars]

# fmsb needs a min & max row:
max_vals <- apply(radar_data, 2, max)
min_vals <- apply(radar_data, 2, min)
radar_chart_data <- rbind(max_vals, min_vals, radar_data)

rownames(radar_chart_data) <- c("Max", "Min", paste("Cluster", 1:optimal_k))

library(openxlsx)

# Specify the file path where you want to save the Excel file
file_path <- "./datos/clusters_EV.xlsx"

# Write the dataframe to an Excel file
write.xlsx(radar_chart_data, file_path, sheetName = "Sheet1", rowNames = FALSE)
write_csv(radar_chart_data, "./datos/clusters_EV.csv")




# We open the file again
radar_chart_data <- read.csv(file="./datos/clusters_EV.csv",
                             sep=",") |> 
  tail(-2)

ex_cluster_EV_1 <- cars |> 
  filter(price>400000& Type == "New" &
           Drivetrain == "4WD" & Body.type == "Coupe" &
           safety_security_elements > 0 &
           vehicle_type == "PHEV")


ex_cluster_EV_2 <- cars |> 
  filter(price<25000 & Type == "Used" &
           240<Power & Power<320 &
           CO..emissions<90 & Empty.weight>1700 & Empty.weight<2200 &
           safety_security_elements > 0)

ex_cluster_EV_2 <- ex_cluster_EV_2 |> slice(2)


ex_cluster_EV_3 <- cars |> 
  filter(price>200000 & Type == "New" & Power>400 &
           Drivetrain == "4WD" & CO..emissions<100 &
           vehicle_type == "PHEV" &
           Empty.weight > 1900 &
           safety_security_elements>0)

ex_cluster_EV_3 <- ex_cluster_EV_3 |> slice(2)


ex_cluster_EV_4 <- cars |> 
  filter(price>80000 & price<130000 & Type == "New" & Power>300 &
           CO..emissions<70 & Drivetrain == "4WD" &
           Empty.weight > 2000 & Engine.size > 2000 &
           safety_security_elements>0)

ex_cluster_EV_4 <- ex_cluster_EV_4 |> slice(3)


ex_cluster_EV_5 <- cars |> 
  filter(price<30000 & price > 25000 & Type == "Used" & Power > 150 &
           CO..emissions<70 & vehicle_type == "BEV" & Mileage > 40000 &
           safety_security_elements>0)

ex_cluster_EV_5 <- ex_cluster_EV_5 |> slice(1)


ex_cluster_EV_6 <- cars |> 
  filter(price>30000 & price<40000 & Type == "New" & Power>150 &
           safety_security_elements>0 & Drivetrain == "Front" &
           CO..emissions<80 & Engine.size < 1500 &
           Body.type == "Off-Road/Pick-up")

ex_cluster_EV_6 <- ex_cluster_EV_6 |> slice(2)

# Hacemos 2 ejemplos de clusters aparte para incluir un coche low-cost
# y otro adicional que puede tener las características del nuestro

ex_cluster_EV_7 <- cars |> 
  filter(price<22000 & Type == "New" & vehicle_type == "BEV" &
           car_make == "Dacia" &
           safety_security_elements>0)

ex_cluster_EV_7 <- ex_cluster_EV_7 |> slice(4)


ex_clusters <- rbind(ex_cluster_EV_1, ex_cluster_EV_2, ex_cluster_EV_3,
                     ex_cluster_EV_4, ex_cluster_EV_5, ex_cluster_EV_6,
                     ex_cluster_EV_7)



library(openxlsx)

# Specify the file path where you want to save the Excel file
file_path <- "./datos/examples_clusters_EV.xlsx"

# Write the dataframe to an Excel file
write.xlsx(ex_clusters, file_path, sheetName = "Sheet1", rowNames = FALSE)
write_csv(ex_clusters, "./datos/examples_clusters_EV.csv")

