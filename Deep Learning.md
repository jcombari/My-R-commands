# R commands

## neural network

library(keras)

mnist <- dataset_mnist()

train_images <- mnist$train$x

train_labels <- mnist$train$y

test_images <- mnist$test$x

test_labels <- mnist$test$y

train_images and train_labels form the training set, the data that the model will learn from. The model will then be tested on the test set, test_images and test_labels. The images are encoded as as 3D arrays, and the labels are a 1D array of digits, ranging from 0 to 9. There is a one-to-one correspondence between the images and the labels.

The R str() function is a convenient way to get a quick glimpse at the structure of an array. Let’s use it to have a look at the training data:

str(train_images)
 int [1:60000, 1:28, 1:28] 0 0 0 0 0 0 0 0 0 0 ...

str(train_labels)
 int [1:60000(1d)] 5 0 4 1 9 2 1 3 1 4 ...

Let’s have a look at the test data:

str(test_images)
 int [1:10000, 1:28, 1:28] 0 0 0 0 0 0 0 0 0 0 ...

str(test_labels)
 int [1:10000(1d)] 7 2 1 0 4 1 4 9 5 9 ...


network <- keras_model_sequential() %>% 

  layer_dense(units = 512, activation = "relu", input_shape = c(28 * 28)) %>% 

  layer_dense(units = 10, activation = "softmax")

The exact purpose of the loss function and the optimizer will be made clear throughout the next two chapters.

network %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)


train_images <- array_reshape(train_images, c(60000, 28 * 28))
train_images <- train_images / 255
test_images <- array_reshape(test_images, c(10000, 28 * 28))
test_images <- test_images / 255


# Overfitting and underfitting

library(keras)
imdb <- dataset_imdb(num_words = 10000)
c(c(train_data, train_labels), c(test_data, test_labels)) %<-% imdb
vectorize_sequences <- function(sequences, dimension = 10000) {
  # Create an all-zero matrix of shape (len(sequences), dimension)
  results <- matrix(0, nrow = length(sequences), ncol = dimension)
  for (i in 1:length(sequences))
    # Sets specific indices of results[i] to 1s
    results[i, sequences[[i]]] <- 1
  results
}
# Our vectorized training data
x_train <- vectorize_sequences(train_data)
# Our vectorized test data
x_test <- vectorize_sequences(test_data)
# Our vectorized labels
y_train <- as.numeric(train_labels)
y_test <- as.numeric(test_labels)


original_model <- keras_model_sequential() %>% 
  layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>% 
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")
original_model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

