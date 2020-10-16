rm(list = objects())

library(dplyr)
library(data.table)

# introducir la ubicacion donde se encuentren los archivos
home_folder <- 'ubicacion'
# introducir el nobre del archivo con el dataset
dataset_file <- file.path(home_folder, 'nombre_archivo_dataset')
# introducir el nombre del archivo del modelo
model_file <- file.path(home_folder, 'nombre_archivo_modelo')
# introducir el nombre del archivo de las variables
features_file <- file.path(home_folder, 'nombre_archivo_variables')

# cargamos las generic utilities que contienen la funcion DrawROCCurve
# este script esta en GitHub
source('generic_utilities.R')


dt <- data.table::fread(dataset_file)

model <-  readRDS(model_file)
features <-  readRDS(features_file)

dt <- datasets$dev

# tests train test and dev obtenidos de la documentacion
dt[, set := dplyr::case_when(
  YEAR_MONTH_DAY <= 20171130 ~ 'train',
  YEAR_MONTH_DAY >= 20171201 & YEAR_MONTH_DAY <= 20180131 ~ 'dev',
  YEAR_MONTH_DAY >= 20180201 ~ 'test'
)]

# hay que añadir la columna train porque se guardo en los nobmres de variables pero no tiene incidencia sobre la performance
dt[, train := 1]

# nos aseguramos de que las variables a usar estén en formato numerico
dt[, (features) := lapply(.SD, as.numeric), .SDcols = features]

mtx <- as.matrix(dt[, features, with = FALSE])
target <- dt[, TARGET_BINARY_TARG]


xgb_matrix <- xgboost::xgb.DMatrix(mtx, label = target)

predictions <- predict(model, xgb_matrix) %T>%
  testthat::expect_length(nrow(dt))

output <- data.table::data.table(
  set = dt[, set],
  target = dt[, TARGET_BINARY_TARG],
  score = predictions
)

DrawROCCurve(validationSet = output[set == 'train'], target = 'target', scores = 'score', title = 'Train Set')
DrawROCCurve(validationSet = output[set == 'dev'], target = 'target', scores = 'score', title = 'Dev Set')
DrawROCCurve(validationSet = output[set == 'test'], target = 'target', scores = 'score', title = 'Test Set')



