#http://environmentalcomputing.net/plotting-with-ggplot-colours-and-symbols/

data(iris)

#Plotting with ggplot: colours and symbols
IrisPlot <- ggplot(iris, aes(Sepal.Length, Petal.Length)) + geom_point() 

IrisBox <- ggplot(iris, aes(Species, Sepal.Length, fill = Species)) + geom_boxplot()

IrisHist <- ggplot(iris, aes(Sepal.Length)) + geom_histogram() 

#Changing the colour of the whole plot or its outline
IrisBox <- ggplot(iris, aes(Species, Sepal.Length)) + geom_boxplot(fill = "blue", colour = "red") 

IrisHist <- ggplot(iris, aes(Sepal.Length)) + geom_histogram(fill = "yellow", colour = "green")

IrisPlot <- ggplot(iris, aes(Petal.Length, Sepal.Length)) + geom_point(colour = "red") 
 
#Using colour to visualise additional variables

IrisPlot <- ggplot(iris, aes(Petal.Length, Sepal.Length, colour = Species)) + geom_point()

IrisBox <- ggplot(iris, aes(Species, Sepal.Length, fill = Species)) + geom_boxplot() 

#Additional continuous variables

IrisPlot.continuous <- ggplot(iris, aes(Petal.Length, Sepal.Length, colour = Sepal.Width)) + geom_point()

print(IrisPlot.continuous)

print(IrisPlot.continuous + scale_colour_gradient(low = "black", high = "white"))

print(IrisPlot.continuous + scale_colour_gradient(low = "green", high = "red"))

print(IrisPlot.continuous + scale_colour_gradient(low = "darkolivegreen1", high = "darkolivegreen"))

#Choosing your own colours for these variables




