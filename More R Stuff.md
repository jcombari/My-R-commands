# R commands

The first function, makeVector creates a special "vector", which is really a list containing a function to

* set the value of the vector
* get the value of the vector
* set the value of the mean
* get the value of the mean
* makeVector <- function(x = numeric()) {
  
      m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

The following function calculates the mean of the special "vector" created with the above function. However, it first checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

#Principal components analysis

# Generate scaled 4*5 matrix with random std normal samples
set.seed(101)
mat <- scale(matrix(rnorm(20), 4, 5))
dimnames(mat) <- list(paste("Sample", 1:4), paste("Var", 1:5))

# Perform PCA
myPCA <- prcomp(mat, scale. = F, center = F)
myPCA$rotation # loadings
myPCA$x # scores

# Perform SVD
mySVD <- svd(mat)
mySVD # the diagonal of Sigma mySVD$d is given as a vector
sigma <- matrix(0,4,4) # we have 4 PCs, no need for a 5th column
diag(sigma) <- mySVD$d # sigma is now our true sigma matrix


# Compare PCA scores with the SVD's U*Sigma
theoreticalScores <- mySVD$u %*% sigma
all(round(myPCA$x,5) == round(theoreticalScores,5)) # TRUE

# Compare PCA loadings with the SVD's V
all(round(myPCA$rotation,5) == round(mySVD$v,5)) # TRUE

# Show that mat == U*Sigma*t(V)
recoverMatSVD <- theoreticalScores %*% t(mySVD$v)
all(round(mat,5) == round(recoverMatSVD,5)) # TRUE

#Show that mat == scores*t(loadings)
recoverMatPCA <- myPCA$x %*% t(myPCA$rotation)
all(round(mat,5) == round(recoverMatPCA,5)) # TRUE

# PCA of the wine data set

wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep=",")

# Name the variables
colnames(wine) <- c("Cvs","Alcohol","Malic acid","Ash","Alcalinity of ash", "Magnesium", "Total phenols", "Flavanoids", "Nonflavanoid phenols", "Proanthocyanins", "Color intensity", "Hue", "OD280/OD315 of diluted wines", "Proline")

# The first column corresponds to the classes
wineClasses <- factor(wine$Cvs)

# Use pairs
pairs(wine[,-1], col = wineClasses, upper.panel = NULL, pch = 16, cex = 0.5)
legend("topright", bty = "n", legend = c("Cv1","Cv2","Cv3"), pch = 16, col = c("black","red","green"),xpd = T, cex = 2, y.intersp = 0.5)

# How to build logistic regression model in R?

data(BreastCancer, package="mlbench")
bc <- BreastCancer[complete.cases(BreastCancer), ]

str(bc)

glm(Class ~ Cell.shape, family="binomial", data = bc)

# remove id column
bc <- bc[,-1]

# convert factors to numeric
for(i in 1:9) {
 bc[, i] <- as.numeric(as.character(bc[, i]))
}

bc$Class <- ifelse(bc$Class == "malignant", 1, 0)
bc$Class <- factor(bc$Class, levels = c(0, 1))

### How to deal with Class Imbalance?

table(bc$Class)


### Iris Data

data(iris)

data <- iris[ iris$Species != 'virginica', ]

data$Species <- droplevels(data$Species)

formula <- Species ~ Petal.Length + Sepal.Length + Petal.Width

pca.logistic <- pcaLogisticR(formula = formula,
                            data = data, n.pc = 2, scale = TRUE,
                            center = TRUE, max.pc = 2)

set.seed(123)

newdata <- iris[sample.int(150, 40), 1:4]

newdata.prediction <- predict(pca.logistic, newdata, type = 'all')

#plot the missing values
nhanes_miss = aggr(nhanes, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(nhanes), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
