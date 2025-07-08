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

library(skimr)
skim(cars)

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

cars <- cars %>%
  drop_na(car_model, Drivetrain, Gearbox, Seats, Doors, Power, Empty.weight)

colnames(cars)[colnames(cars) == 'Monthly.rate'] <- 'Finance'

cars <- cars |> relocate(days_since_reg, .after = Electric.Range..EAER..7)

cars <- cars |> 
  mutate(
    car_name = paste(car_make, car_model, sep = " ")
  )

cars <- cars |> relocate(car_name, .before = car_make)
cars <- cars |> relocate(X_src, .before = car_name)

cars <- cars |> select(-Production.date)

# We save the dataframe

library(openxlsx)

# Specify the file path where you want to save the Excel file
file_path <- "./datos/db_merged_expanded_2_modified.xlsx"

# Write the dataframe to an Excel file
write.xlsx(cars, file_path, sheetName = "Sheet1", rowNames = FALSE)
write_csv(cars, "./datos/db_merged_expanded_2_modified.csv")


# Dataframe saved

cars2 <- cars

cars2$Previous.owner[is.na(cars2$Previous.owner)] <- 1

cars2 <- cars2 |>
  slice(-order(Previous.owner, decreasing = TRUE)[1:2])

cars2 <- cars2 |> drop_na(Fuel.type, Engine.size)
# 7436 observaciones

cars2 %>%
  group_by(Fuel.type, car_make) %>%
  summarise(
    mean_CO_emissions = mean(CO..emissions, na.rm = TRUE),
    median_CO_emissions = median(CO..emissions, na.rm = TRUE),
    sd_CO_emissions = sd(CO..emissions, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print(n = 40)

cars2 <- cars2 |> 
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

cars2 <- cars2 |>
  slice(-order(CO..emissions, decreasing = TRUE)[1:2])

cars2 <- cars2 |>
  slice(-order(price, decreasing = TRUE)[1:2])

cars2 <- cars2 |>
  slice(-order(Mileage, decreasing = TRUE)[1])

cars2 <- cars2 |>
  slice(-order(Empty.weight, decreasing = TRUE)[1])

cars2 <- cars2 |>
  slice(-order(Empty.weight, decreasing = FALSE)[1])

cars2 <- cars2 |> 
  mutate(CO..emissions = ifelse(
    is.na(CO..emissions), median(CO..emissions, na.rm = TRUE), CO..emissions))

# Mean goes down from 126.3566 to 125.6549

cars2 <- cars2 |> 
  mutate(Electric.Range..EAER. = ifelse(!is.na(Electric.Range..EAER.),1,0),
         Electric.Range = ifelse(!is.na(Electric.Range),1,0),
         Charging.time.from.10..to.80. = 
           ifelse(!is.na(Charging.time.from.10..to.80.),1,0),
         Electric.Range.7 = ifelse(!is.na(Electric.Range.7),1,0),
         Electric.Range..EAER..7 = ifelse(!is.na(Electric.Range..EAER..7),1,0))

cars2 <- cars2 |> select(-X_src)

tabla <- cars2 |>
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
cars2$car_make_category <- lookup_table[cars2$car_make]

cars2 <- cars2 |> relocate(car_make_category, .before = price)

cars2 <- cars2 |> 
  mutate(car_make_category =
           ifelse(is.na(car_make_category),
                  ifelse(price<=19900, "low_cost",
                         ifelse(price>19900&price<=28990, "standard",
                                ifelse(price>28990&price<=49900, "upper_standard",
                                       "premium"))), car_make_category))

# cars2 <- drop_na(cars2)
# cars2 <- cars2 %>%
#   drop_na(Fuel.type, Engine.size, CO..emissions, Previous.owner)
# Electric.Range..EAER., Electric.Range, Charging.time.from.10..to.80.,
# Electric.Range.7, Electric.Range..EAER..7


# We save again all the changes

library(openxlsx)

# Specify the file path where you want to save the Excel file
file_path <- "./datos/db_preprocessed.xlsx"

# Write the dataframe to an Excel file
write.xlsx(cars2, file_path, sheetName = "Sheet1", rowNames = FALSE)
write_csv(cars2, "./datos/db_preprocessed.csv")


# We open the file

cars <- read.csv(file="./datos/db_preprocessed.csv",
                  sep=",")

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

# c("Power", "car_make.Opel", "Mileage", "Fuel.type.Others")

dput(lista[[2]][[2]])
# c("Power", "car_make.Opel", "Mileage", "Fuel.type.Others", "car_make.Ferrari")

dput(lista[[2]][[3]])
# c("Power", "car_make.Opel", "Mileage", "Fuel.type.Others", "car_make.Lamborghini", "car_make.Ferrari")

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



#                                                              %IncMSE  IncNodePurity
# Power                                                   25.549692048  1.052159e+13
# Engine.size                                              9.870604421  3.589605e+12
# Mileage                                                 11.227258206  1.879462e+12
# Empty.weight                                            21.414024513  1.443799e+12
# days_since_reg                                          17.005659943  1.406796e+12
# Seats                                                   11.978858506  1.122770e+12
# CO..emissions                                           10.929488633  1.092244e+12
# car_make_category.premium                                9.600728073  9.447799e+11
# Drivetrain.Front                                         5.524160502  7.334576e+11
# Drivetrain.4WD                                           7.976070728  7.133727e+11
# ratings_number                                          11.185519875  5.428670e+11
# Availability.120                                        12.631773774  3.749041e+11
# Doors                                                   10.045558844  3.709119e+11
# Type.Used                                                6.589733692  3.428435e+11
# Previous.owner                                           6.341433009  2.933920e+11
# Type.New                                                 5.276454033  2.580803e+11
# `Fuel.type.Electric/Gasoline`                            5.549240811  2.503380e+11
# `Upholstery.Full leather`                                5.539332955  1.761833e+11
# Availability.Unknown                                     8.924659512  1.655186e+11
# Other.fuel.types.None                                    6.130336409  1.564053e+11
# Other.fuel.types.Electricity                             5.029484196  1.426468e+11
# Upholstery.colour.Beige                                 -1.032572079  1.060560e+11
# Fuel.type.Diesel                                         5.238485833  1.016570e+11
# Upholstery.colour.Black                                  3.980657604  1.011165e+11
# Availability.42                                          6.611038355  9.649684e+10
# Charging.time.from.10..to.80.                            4.199263451  9.615629e+10
# Body.type.Coupe                                          4.123789425  9.488700e+10
# Fuel.type.Gasoline                                       7.394653433  9.289115e+10
# Upholstery.Cloth                                         4.443201704  8.705281e+10
# Drivetrain.Rear                                          4.946396792  8.573369e+10
# Body.type.Sedan                                          5.936470337  8.492376e+10
# Electric.Range                                           4.347555793  8.434355e+10
# General.inspection                                       5.299153210  8.391501e+10
# Body.type.Convertible                                    2.687444410  7.839863e+10
# Electric.Range..EAER..7                                  4.957929434  6.930818e+10
# Emissions.sticker.Unknown                                3.154442258  6.770685e+10
# Availability.90                                          3.053375988  6.588258e+10
# Electric.Range.7                                         3.599231303  6.142383e+10
# `Body.type.Off-Road/Pick-up`                             5.348555774  5.836717e+10
# CO..efficiency                                           7.453346986  5.641519e+10
# Upholstery.Unknown                                       2.211740984  5.559724e+10
# Fuel.consumption                                         6.155595957  5.347969e+10
# car_make_category.standard                               4.087224970  5.167886e+10
# Electric.Range..EAER.                                    6.339222095  5.156860e+10
# Gearbox.Manual                                           5.194874311  5.094989e+10
# `Emission.class.Euro 6d`                                 5.570647835  4.888318e+10
# Energy.efficiency.class.Unknown                          6.738019322  4.628617e+10
# Non.smoker.vehicle                                       3.340743661  4.625514e+10
# Emission.class.Unknown                                   2.810506379  4.569894e+10
# Full.service.history                                     5.953422843  4.230678e+10
# car_make_category.upper_standard                         6.117639600  4.158030e+10
# `Emission.class.Euro 6c`                                 1.312941345  4.117460e+10
# Body.type.Van                                           10.119651352  4.004052e+10
# `Emissions.sticker.4 (Green)`                            3.206857396  3.699745e+10
# Gearbox.Automatic                                        4.308637840  3.697273e+10
# Colour.White                                             4.046740665  3.497132e+10
# Upholstery.colour.Red                                   -1.743203980  3.475762e+10
# Energy.efficiency.class.G                               -1.932397373  3.352054e+10
# Paint.Unknown                                            3.940013771  3.322989e+10
# Warranty                                                 2.639693175  3.268833e+10
# `Emission.class.Euro 6d-TEMP`                            2.582980609  3.196530e+10
# Paint.Metallic                                           3.007499016  3.144321e+10
# Colour.Black                                             2.919785308  2.972821e+10
# `Emission.class.Euro 5`                                  3.842367250  2.795172e+10
# `Upholstery.Part leather`                                2.299562828  2.625078e+10
# `Emission.class.Euro 6`                                  4.242014262  2.604017e+10
# Colour.Orange                                            1.165568058  2.593155e+10
# Upholstery.colour.Other                                  2.261042025  2.544141e+10
# Paint.Others                                             2.433819108  2.474852e+10
# Colour.Red                                               1.373887257  2.344780e+10
# Energy.efficiency.class.C                                4.092645626  2.221507e+10
# Colour.Blue                                              2.924889768  2.211677e+10
# Upholstery.alcantara                                     2.601170383  2.199826e+10
# Upholstery.colour.Unknown                                1.977759005  2.079490e+10
# Colour.Grey                                              3.216292922  1.944349e+10
# Upholstery.colour.Brown                                  1.327738497  1.757342e+10
# Colour.Green                                             2.267300313  1.726462e+10
# Colour.Violet                                           -1.122458269  1.509764e+10
# Colour.Yellow                                           -0.498730605  1.444748e+10
# Energy.efficiency.class.A                                2.695117487  1.397310e+10
# Fuel.type.Others                                         1.470632874  1.197716e+10
# Upholstery.colour.Grey                                   2.503305886  1.165516e+10
# `Emission.class.Euro 6e`                                 1.693938146  1.109471e+10
# entertainment_media_elements                             0.361688451  1.086969e+10
# Fuel.type.Electric                                       5.380732388  1.075921e+10
# Fuel.type.LPG                                            4.374799946  1.073638e+10
# Colour.Gold                                              1.803901660  1.071334e+10
# comfort_convenience_Electrically.heated.windshield       1.422275775  1.013047e+10
# Colour.Silver                                            1.101235206  1.001261e+10
# `Body.type.Station wagon`                                3.624373509  8.905701e+09
# extras_Spare.tyre                                        0.630408901  8.533078e+09
# `Emission.class.Euro 4`                                  7.572153376  8.322789e+09
# entertainment_media_Television                           1.172422263  8.298436e+09
# extras_elements                                          1.090129383  8.100379e+09
# comfort_convenience_elements                             2.287604184  7.836756e+09
# Upholstery.colour.Blue                                  -0.248489973  7.778310e+09
# comfort_convenience_Air.suspension                       0.951118222  7.759810e+09
# comfort_convenience_Automatic.climate.control            1.184464816  7.273912e+09
# Upholstery.Other                                         2.430119424  6.944938e+09
# safety_security_Night.view.assist                        0.857036288  6.923636e+09
# comfort_convenience_Electric.backseat.adjustment        -0.676792167  6.866369e+09
# Upholstery.colour.White                                 -0.715952632  6.425369e+09
# safety_security_elements                                 0.608296777  6.173314e+09
# comfort_convenience_Massage.seats                        0.250489041  5.989846e+09
# `Emissions.sticker.1 (No sticker)`                       3.282801282  5.879918e+09
# comfort_convenience_Air.conditioning                     1.150419242  5.679673e+09
# Body.type.Compact                                        3.194064662  5.382363e+09
# safety_security_Full.LED.headlights                      1.668698984  4.936760e+09
# Availability.180                                         1.049319253  4.713860e+09
# comfort_convenience_Tinted.windows                       0.441911824  4.575852e+09
# comfort_convenience_Parking.assist.system.sensors.front  0.154909173  4.399854e+09
# Battery.Ownership.Included                               0.732858608  4.365568e+09
# extras_Catalytic.Converter                               1.067021148  4.263697e+09
# Availability.7                                           2.484045953  4.203616e+09
# comfort_convenience_Automatic.climate.control..4.zones   2.698088992  4.187462e+09
# safety_security_Fog.lights                               1.482212938  3.813305e+09
# entertainment_media_CD.player                            2.895949436  3.714787e+09
# Energy.efficiency.class.E                               -1.729916592  3.632386e+09
# comfort_convenience_Parking.assist.system.camera         1.109523917  3.607651e+09
# Energy.efficiency.class.B                                1.670173821  3.508995e+09
# entertainment_media_Android.Auto                         1.164327938  3.401432e+09
# comfort_convenience_Sunroof                              1.272118199  3.347091e+09
# comfort_convenience_Cruise.control                       1.629943618  3.333939e+09
# safety_security_LED.Daytime.Running.Lights               1.183388910  3.317224e+09
# extras_Summer.tyres                                     -0.725205237  3.316750e+09
# entertainment_media_WLAN...WiFi.hotspot                  1.435456769  3.312138e+09
# comfort_convenience_Parking.assist.system.self.steering  0.845727932  2.923044e+09
# comfort_convenience_360..camera                         -0.928287461  2.875462e+09
# comfort_convenience_Automatic.climate.control..2.zones   0.244396195  2.771647e+09
# entertainment_media_Digital.radio                        0.323953733  2.754730e+09
# entertainment_media_MP3                                  0.478152130  2.753338e+09
# extras_E10.enabled                                      -1.062196021  2.698814e+09
# comfort_convenience_Heads.up.display                    -0.125024540  2.663914e+09
# Colour.Brown                                            -1.290462776  2.632390e+09
# Availability.150                                         1.159783432  2.605611e+09
# entertainment_media_Hands.free.equipment                 0.229531323  2.575372e+09
# comfort_convenience_Hill.Holder                          1.796001155  2.443871e+09
# safety_security_LED.Headlights                          -0.571988450  2.440708e+09
# extras_Headlight.washer.system                           0.527453222  2.377962e+09
# comfort_convenience_Electrically.adjustable.seats        1.573024520  2.366061e+09
# extras_Electronic.parking.brake                          1.143437135  2.363174e+09
# comfort_convenience_Start.stop.system                   -0.025164931  2.315116e+09
# entertainment_media_USB                                 -0.090481447  2.196030e+09
# safety_security_Emergency.system                        -0.905119801  2.137353e+09
# extras_Alloy.wheels                                      0.305764787  2.083604e+09
# entertainment_media_Integrated.music.streaming           0.533949489  2.077287e+09
# extras_Sport.suspension                                 -1.145857120  2.071161e+09
# car_make_category.low_cost                               2.315622679  1.967733e+09
# extras_Winter.package                                   -0.198774473  1.917674e+09
# Finance                                                  2.034776947  1.893509e+09
# safety_security_Blind.spot.monitor                       1.038926459  1.835556e+09
# Colour.Unknown                                           6.190817937  1.792624e+09
# safety_security_Isofix                                   1.315509302  1.781103e+09
# extras_Trailer.hitch                                     1.965924650  1.755383e+09
# safety_security_Alarm.system                            -1.100209264  1.749644e+09
# Availability.2                                           1.618013051  1.747843e+09
# safety_security_Head.airbag                              0.962930928  1.690720e+09
# `Energy.efficiency.class.A+`                             1.419017674  1.654544e+09
# safety_security_ABS                                     -1.048376819  1.599792e+09
# safety_security_Daytime.running.lights                   1.914820402  1.599165e+09
# extras_Roof.rack                                         1.440777864  1.594043e+09
# comfort_convenience_Electric.tailgate                    1.485548404  1.566726e+09
# comfort_convenience_Panorama.roof                        1.745066186  1.560835e+09
# comfort_convenience_Auxiliary.heating                   -0.766725907  1.539877e+09
# extras_Sport.package                                     2.273878448  1.494032e+09
# safety_security_Emergency.brake.assistant                0.354425004  1.483673e+09
# safety_security_Central.door.lock                        2.055487634  1.460824e+09
# extras_Emergency.tyre.repair.kit                        -0.266248209  1.454329e+09
# comfort_convenience_Power.windows                       -0.250709267  1.424596e+09
# comfort_convenience_Seat.heating                         1.237948591  1.415490e+09
# comfort_convenience_Rain.sensor                          1.907280124  1.405990e+09
# safety_security_Glare.free.high.beam.headlights          2.553755288  1.390611e+09
# comfort_convenience_Seat.ventilation                     0.660258317  1.381085e+09
# Body.type.Other                                          1.607233761  1.377952e+09
# safety_security_Traffic.sign.recognition                -0.857438597  1.373179e+09
# comfort_convenience_Fold.flat.passenger.seat             1.099585232  1.319765e+09
# safety_security_Central.door.lock.with.remote.control    0.790207659  1.303035e+09
# entertainment_media_Digital.cockpit                      1.036485007  1.299417e+09
# extras_Spoiler                                          -0.464585893  1.297044e+09
# Availability.14                                          1.923844227  1.260593e+09
# Energy.efficiency.class.D                                1.699867881  1.246446e+09
# comfort_convenience_Rear.seat.heating                   -1.137256451  1.212562e+09
# extras_Voice.Control                                     1.241468432  1.196700e+09
# comfort_convenience_Navigation.system                   -1.526717553  1.121556e+09
# comfort_convenience_Keyless.central.door.lock            1.898586743  1.114502e+09
# safety_security_Adaptive.headlights                     -0.553161555  1.096463e+09
# extras_Ambient.lighting                                  2.400967862  1.067065e+09
# extras_Cargo.barrier                                     0.938412411  1.064185e+09
# safety_security_Passenger.side.airbag                   -0.208647223  1.030978e+09
# Battery.Ownership.Unknown                                2.032636758  1.023289e+09
# `Energy.efficiency.class.A+++`                          -1.530004831  9.982190e+08
# safety_security_Bi.Xenon.headlights                      0.237779585  9.833566e+08
# safety_security_Lane.departure.warning.system            2.230255213  9.819664e+08
# extras_Sport.seats                                      -0.844349858  9.588178e+08
# safety_security_Adaptive.Cruise.Control                 -0.194344169  9.185295e+08
# safety_security_Speed.limit.control.system               1.539537765  9.135516e+08
# comfort_convenience_Heated.steering.wheel                2.249013619  8.853889e+08
# comfort_convenience_Leather.steering.wheel              -1.185842898  8.828542e+08
# entertainment_media_Induction.charging.for.smartphones   1.519514665  8.790790e+08
# extras_Touch.screen                                      0.744228862  8.629093e+08
# comfort_convenience_Leather.seats                       -0.860700628  8.522301e+08
# entertainment_media_Sound.system                        -0.942936866  8.515870e+08
# safety_security_Side.airbag                             -0.614277713  8.454303e+08
# safety_security_Traction.control                         1.729924150  7.838361e+08
# Energy.efficiency.class.F                                1.368401720  7.729012e+08
# safety_security_Xenon.headlights                        -0.896395225  7.439412e+08
# comfort_convenience_Parking.assist.system.sensors.rear   1.210378427  7.287827e+08
# safety_security_High.beam.assist                         1.137492440  7.133901e+08
# comfort_convenience_None                                 2.335765455  6.975836e+08
# safety_security_Driver.drowsiness.detection              1.260967875  6.674370e+08
# entertainment_media_None                                -1.004139933  6.199461e+08
# extras_Shift.paddles                                     1.566724701  6.156269e+08
# comfort_convenience_Electrical.side.mirrors             -1.215049891  6.120977e+08
# extras_All.season.tyres                                  0.394144705  6.068831e+08
# safety_security_Distance.warning.system                  1.409501722  5.955217e+08
# comfort_convenience_Split.rear.seats                     1.604552389  5.926948e+08
# comfort_convenience_Automatic.climate.control..3.zones  -0.190006836  5.792618e+08
# extras_None                                             -0.072757818  5.705179e+08
# comfort_convenience_Armrest                             -0.349511841  5.683849e+08
# extras_Particle.filter                                   2.685952712  5.682828e+08
# Availability.1                                           0.008077885  5.553330e+08
# extras_Automatically.dimming.interior.mirror             2.364992566  5.526806e+08
# safety_security_Electronic.stability.control             1.864886122  5.514666e+08
# Fuel.type.CNG                                            3.092381872  5.440350e+08
# entertainment_media_Radio                                1.304084415  5.395610e+08
# entertainment_media_Bluetooth                           -0.271747905  5.292892e+08
# safety_security_Rear.airbag                              0.866750166  5.257806e+08
# comfort_convenience_Light.sensor                        -1.071710820  5.221178e+08
# Availability.270                                         0.450081438  5.106009e+08
# safety_security_None                                     0.535228389  5.069494e+08
# entertainment_media_On.board.computer                   -1.162010322  5.047719e+08
# comfort_convenience_Lumbar.support                      -0.331378473  4.999439e+08
# safety_security_Driver.side.airbag                       1.435679038  4.946119e+08
# entertainment_media_Apple.CarPlay                        1.184383242  4.697244e+08
# comfort_convenience_Multi.function.steering.wheel        0.189873134  4.541503e+08
# extras_Alloy.wheels..21..                                1.397177475  4.539336e+08
# Colour.Beige                                             0.712011669  4.473340e+08
# Upholstery.colour.Orange                                 1.505322399  4.326193e+08
# Upholstery.Velour                                        0.974083241  4.079799e+08
# Battery.certificate                                      0.332679796  4.029522e+08
# extras_Emergency.tyre                                   -0.218633238  3.721091e+08
# safety_security_Power.steering                          -1.412920738  3.594747e+08
# extras_Alloy.wheels..20..                                0.483625688  3.447951e+08
# comfort_convenience_Sliding.door.left                    2.105978744  3.373038e+08
# safety_security_Tire.pressure.monitoring.system          0.882209771  3.283201e+08
# extras_Alloy.wheels..22..                                1.199791080  3.255732e+08
# comfort_convenience_Park.Distance.Control                1.163683021  3.222267e+08
# safety_security_Immobilizer                              2.030795457  3.041027e+08
# extras_Steel.wheels                                      3.579328029  2.773645e+08
# comfort_convenience_Sliding.door.right                   1.160064202  2.473953e+08
# safety_security_Laser.headlights                        -0.861012429  2.293544e+08
# extras_Smoker.s.package                                 -1.148421053  2.288090e+08
# extras_Ski.bag                                           0.854397521  2.200405e+08
# Upholstery.colour.Green                                 -1.001670845  1.856001e+08
# extras_Alloy.wheels..19..                                0.686868668  1.356323e+08
# comfort_convenience_Wind.deflector                       0.000000000  1.340662e+08
# extras_Winter.tyres                                     -0.152076627  1.225946e+08
# extras_Alloy.wheels..23..                                0.000000000  1.206041e+08
# Availability.5                                           1.503236268  1.130278e+08
# extras_Tuned.car                                        -0.707143838  9.906284e+07
# Availability.28                                         -1.639623465  9.226626e+07
# Availability.3                                           0.138744207  8.785819e+07
# Availability.60                                          1.202774950  8.216537e+07
# extras_Alloy.wheels..17..                               -1.063702647  4.887732e+07
# extras_Handicapped.enabled                               1.345681122  4.759688e+07
# Body.type.Transporter                                   -0.885513868  4.723012e+07
# `Emission.class.Euro 3`                                 -0.493176348  3.651553e+07
# `Emission.class.Euro 6b`                                 5.268254327  2.732594e+07
# extras_Alloy.wheels..18..                               -0.978489383  2.672410e+07
# Availability.4                                           0.000000000  2.303763e+07
# extras_Awning                                            1.001670845  2.098835e+07
# extras_Alloy.wheels..16..                                2.492640192  2.058265e+07
# `Emission.class.Euro 2`                                  1.001670845  1.689691e+07
# Availability.360                                         4.040063295  1.598718e+07
# extras_Sliding.door                                      1.001670845  1.557017e+07
# extras_Heat.pump                                        -1.551138042  1.460186e+07
# extras_Alloy.wheels..15..                                0.581491268  1.435057e+07
# extras_Range.extender                                   -1.008433438  1.348177e+07
# `Energy.efficiency.class.A++`                            1.902330923  9.893197e+06
# Battery.Ownership.Rented                                 1.593124789  8.312249e+06
# Colour.Bronze                                            1.237728922  8.024224e+06
# extras_Right.hand.drive                                  0.000000000  7.140931e+06
# extras_Alloy.wheels..12..                                0.000000000  4.185485e+06
# `Emission.class.Euro 1`                                  1.001670845  2.685383e+06
# `Emissions.sticker.3 (Yellow)`                           0.000000000  2.106151e+06
# `Gearbox.Semi-automatic`                                 0.000000000  1.109059e+06
# extras_Alloy.wheels..14..                                0.000000000  3.718435e+05
# Availability.6                                           0.000000000  2.577556e+05
# extras_Bidirectional.charging                            0.000000000  0.000000e+00
# extras_Differential.lock                                 0.000000000  0.000000e+00
# extras_Divider                                           0.000000000  0.000000e+00
# extras_Municipal.vehicle                                 0.000000000  0.000000e+00


### We select the following variables

selected_vars <- c("price", "Power", "Engine.size", "Mileage", "Empty.weight",
                   "days_since_reg", "Seats", "CO..emissions",
                   "car_make_category", "Drivetrain", "ratings_number",
                   "Availability", "Doors", "Type", "Previous.owner",
                   "Fuel.type", "Body.type")

cars <- cars |> select(all_of(selected_vars))

cars <- cars |> mutate(price = min_max(price))

corr <- cor(cars[,num_cols], use="pairwise.complete.obs")

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
                     grepl("(28|90|60|1|3|7|42|14|2|5|4|6)",
                           Availability, ignore.case = FALSE) ~ "<120"))

### We do the clustering process

library(factoextra)

var_cuali <- c("car_make_category", "Drivetrain", "Availability",
               "Type", "Fuel.type", "Body.type")

cars2 <- dummy.data.frame(cars, var_cuali, sep = ".")

fviz_nbclust(cars2, kmeans, method="wss")       # elbow method 8 clusters or 10
fviz_nbclust(cars2, kmeans, method="silhouette") # 2 or 7 clusters




optimal_k <- 8   # for example, choose k = 8
set.seed(123)
km_final <- kmeans(cars2, centers = optimal_k, nstart = 25)
table(km_final$cluster)

# View cluster centers on the original (unscaled) variables:
cluster_profiles <- aggregate(cars, 
                              by = list(cluster = km_final$cluster), 
                              FUN = mean)
print(cluster_profiles)

# Alta relación entre mileage, days_since_reg, Type y Previous.owner


# (You might pick, say, 5–7 features to highlight on a radar plot.)
library(fmsb)

# Example: select 6 variables of interest 
radar_vars <- c("price", "Engine.size", "Mileage", "Power",
                "Empty.weight", "ratings_number", "CO..emissions",
                "Drivetrain.Front", "Drivetrain.4WD",
                "Fuel.type.Gasoline", "Fuel.type.Diesel",
                "Fuel.type.Electric/Gasoline",
                "Body.type.Off-Road/Pick-up", "Body.type.Sedan",
                "Body.type.Compact")
radar_data <- cluster_profiles[, radar_vars]

# fmsb needs a min & max row:
max_vals <- apply(radar_data, 2, max)
min_vals <- apply(radar_data, 2, min)
radar_chart_data <- rbind(max_vals, min_vals, radar_data)

rownames(radar_chart_data) <- c("Max", "Min", paste("Cluster", 1:optimal_k))

# Plot radar
radarchart(radar_chart_data, 
           axistype = 1, 
           seg      = 4, 
           plty     = 1:4,      # line types per cluster
           pcol     = 1:4,      # colors per cluster
           title    = "Cluster Profiles (Radar Chart)")
legend("topright", legend = paste("Cluster", 1:optimal_k), col = 1:4, lty = 1:4, cex = 0.8)

# https://online.visual-paradigm.com/app/diagrams/#infoart:proj=0&type=RadarCharts&gallery=/repository/f87c4917-1cd2-4055-9d5b-5ac51d035a5a.xml&name=Radar%20Chart




library(openxlsx)

# Specify the file path where you want to save the Excel file
file_path <- "./datos/clusters.xlsx"

# Write the dataframe to an Excel file
write.xlsx(radar_chart_data, file_path, sheetName = "Sheet1", rowNames = FALSE)
write_csv(radar_chart_data, "./datos/clusters.csv")


# We open the file again
radar_chart_data <- read.csv(file="./datos/clusters.csv",
                             sep=",")
radar_chart_data <-radar_chart_data |> 
  tail(-2)

ex_cluster_1 <- cars |> 
  filter(45000<price & price<55000 &
           Engine.size<3000 & Type == "Used" &
           Mileage>60000 &
           Body.type == "Sedan")
ex_cluster_1 <- ex_cluster_1 |> slice(8)


ex_cluster_2 <- cars |> 
  filter(22000<price & price<30000 & Type == "New" &
           Drivetrain == "Front" & 100<Power & Power<200 &
           CO..emissions<120 & Empty.weight>1200 & Empty.weight<1600 &
           50<ratings_number & ratings_number<150 &
           Fuel.type == "Electric/Gasoline" &
           Body.type == "Off-Road/Pick-up" & car_make != "MG")

ex_cluster_2 <- ex_cluster_2 |> slice(3)


ex_cluster_3 <- cars |> 
  filter(17000<price & price<22000 & Type == "New" & Power<130 &
           Drivetrain == "Front" & Fuel.type == "Gasoline" &
           CO..emissions>100 & CO..emissions<140 &
           Body.type != "Off-Road/Pick-up" &
           Empty.weight < 1400 & ratings_number>90 &
           safety_security_elements>0)

ex_cluster_3 <- ex_cluster_3 |> slice(6)


ex_cluster_4 <- cars |> 
  filter(17000<price & price<22000 & Type == "Used" & Power>100 &
           Power<150 & CO..emissions>80 & CO..emissions<120 &
           Drivetrain == "Front" & safety_security_elements>0)

ex_cluster_4 <- ex_cluster_4 |> slice(3)


ex_cluster_5 <- cars |> 
  filter(28000<price & price<45000 & Type == "New" & Power > 150 &
           CO..emissions<140 &
           Drivetrain == "Front" & Fuel.type == "Gasoline" &
           safety_security_elements>0)

ex_cluster_5 <- ex_cluster_5 |> slice(2)


ex_cluster_6 <- cars |> 
  filter(15000<price & price<20000 & Type == "New" & Power<100 &
           safety_security_elements>0 & Drivetrain == "Front" &
           CO..emissions<100 & Fuel.type == "Diesel" &
           Body.type == "Compact")

ex_cluster_6 <- ex_cluster_6 |> slice(3)


ex_cluster_7 <- cars |> 
  filter(price>50000 & price<70000 & Type == "Used" &
           Engine.size>2200 & Empty.weight>1500 & Power >250 &
           Drivetrain == "4WD" & safety_security_elements>0 &
           Mileage>60000 & Mileage<100000)

ex_cluster_7 <- ex_cluster_7 |> slice(4)


ex_cluster_8 <- cars |> 
  filter(price>80000 & Type == "New" &
           Engine.size>2200 & Empty.weight>1500 & Power >300 &
           Drivetrain == "4WD" & safety_security_elements>0 &
           car_make == "Ferrari")


ex_clusters <- rbind(ex_cluster_1, ex_cluster_2, ex_cluster_3,
                     ex_cluster_4, ex_cluster_5, ex_cluster_6,
                     ex_cluster_7, ex_cluster_8)



library(openxlsx)

# Specify the file path where you want to save the Excel file
file_path <- "./datos/examples_clusters.xlsx"

# Write the dataframe to an Excel file
write.xlsx(ex_clusters, file_path, sheetName = "Sheet1", rowNames = FALSE)
write_csv(ex_clusters, "./datos/examples_clusters.csv")

