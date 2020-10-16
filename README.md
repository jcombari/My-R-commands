# R commands

## Input and display

### read files with labels in first row
read.table(filename,header=TRUE)           #read a tab or space delimited file
read.table(filename,header=TRUE,sep=',')   #read csv files

x <- c(1,2,4,8,16 )                           #create a data vector with specified elements

y <- c(1:10)                                  #create a data vector with elements 1-10

n <- 10

x1 <- c(rnorm(n))                             #create a n item vector of random normal deviates

y1 <- c(runif(n))+n                           #create another n item vector that has n added to each random uniform distribution

z <- rbinom(n,size,prob)                      #create n samples of size "size" with probability prob from the binomial

vect <- c(x,y)                                #combine them into one vector of length 2n

mat <- cbind(x,y)                             #combine them into a n x 2 matrix

mat[4,2]                                   #display the 4th row and the 2nd column

mat[3,]                                    #display the 3rd row

mat[,2]                                    #display the 2nd column

subset(dataset,logical)                    #those objects meeting a logical criterion

subset(data.df,select=variables,logical)   #get those objects from a data frame that meet a criterion

data.df[data.df=logical]                   #yet another way to get a subset

x[order(x$B),]                             #sort a dataframe by the order of the elements in B

x[rev(order(x$B)),]                        #sort the dataframe in reverse order 

browse.workspace                           #a Mac menu command that creates a window with information about all variables in the workspace


## Moving around

ls()                                      #list the variables in the workspace

rm(x)                                     #remove x from the workspace

rm(list=ls())                             #remove all the variables from the workspace

attach(mat)                               #make the names of the variables in the matrix or data frame available in the workspace

detach(mat)                               #releases the names (remember to do this each time you attach something)

with(mat, .... )                          #a preferred alternative to attach ... detach

new <- old[,-n]                              #drop the nth column

new <- old[-n,]                              #drop the nth row

new <- old[,-c(i,j)]                      #drop the ith and jth column

new <- subset(old,logical)                   #select those cases that meet the logical condition

complete  <-  subset(data.df,complete.cases(data.df)) #find those cases with no missing values

new <- old[n1:n2,n3:n4]                      #select the n1 through n2 rows of variables n3 through n4)

## Distributions

beta(a, b)

gamma(x)

choose(n, k)

factorial(x)

dnorm(x, mean=0, sd=1, log = FALSE)      #normal distribution

pnorm(q, mean=0, sd=1, lower.tail = TRUE, log.p = FALSE)

qnorm(p, mean=0, sd=1, lower.tail = TRUE, log.p = FALSE)

rnorm(n, mean=0, sd=1)

dunif(x, min=0, max=1, log = FALSE)      #uniform distribution

punif(q, min=0, max=1, lower.tail = TRUE, log.p = FALSE)

qunif(p, min=0, max=1, lower.tail = TRUE, log.p = FALSE)

runif(n, min=0, max=1)

# Data manipulation

replace(x, list, values)                 #remember to assign this to some object i.e., x <- replace(x,x==-9,NA) 
                                         #similar to the operation x[x==-9] <- NA
scrub(x, where, min, max, isvalue,newvalue)  #a convenient way to change particular values (in psych package)

cut(x, breaks, labels = NULL,
    include.lowest = FALSE, right = TRUE, dig.lab = 3, ...)

x.df <- data.frame(x1,x2,x3 ...)             #combine different kinds of data into a data frame
	    as.data.frame()
	     is.data.frame()

x <- as.matrix()

scale()                                   #converts a data frame to standardized scores

round(x,n)                                #rounds the values of x to n decimal places

ceiling(x)                                #vector x of smallest integers > x

floor(x)                                  #vector x of largest interger < x

as.integer(x)                             #truncates real x to integers (compare to round(x,0)

as.integer(x < cutpoint)                  #vector x of 0 if less than cutpoint, 1 if greater than cutpoint)

factor(ifelse(a < cutpoint, "Neg", "Pos"))  #is another way to dichotomize and to make a factor for analysis 

transform(data.df,variable names = some operation) #can be part of a set up for a data set 

x%in%y                     #tests each element of x for membership in y

y%in%x                     #tests each element of y for membership in x

all(x%in%y)                #true if x is a proper subset of y

all(x)                     # for a vector of logical values, are they all true?

any(x)                     #for a vector of logical values, is at least one true?

# Statistics and transformations

max(x, na.rm=TRUE)     #Find the maximum value in the vector x, exclude missing values

min(x, na.rm=TRUE)

mean(x, na.rm=TRUE)

median(x, na.rm=TRUE)

sum(x, na.rm=TRUE)

var(x, na.rm=TRUE)     #produces the variance covariance matrix

sd(x, na.rm=TRUE)      #standard deviation

mad(x, na.rm=TRUE)    #(median absolute deviation)

fivenum(x, na.rm=TRUE) #Tukey fivenumbers min, lowerhinge, median, upper hinge, max

table(x)    #frequency counts of entries, ideally the entries are factors(although it works with integers or even reals)

scale(data,scale=FALSE)   #centers around the mean but does not scale by the sd)

cumsum(x,na=rm=TRUE)     #cumulative sum, etc.

cumprod(x)

cummax(x)

cummin(x)

rev(x)      #reverse the order of values in x
 
cor(x,y,use="pair")   #correlation matrix for pairwise complete data, use="complete" for complete cases

aov(x~y,data=datafile)  #where x and y can be matrices

aov.ex1 = aov(DV~IV,data=data.ex1)  #do the analysis of variance or

aov.ex2 = aov(DV~IV1*IV21,data=data.ex2)         #do a two way analysis of variance

summary(aov.ex1)                                    #show the summary table

print(model.tables(aov.ex1,"means"),digits=3)       #report the means and the number of subjects/cell

boxplot(DV~IV,data=data.ex1)        #graphical summary appears in graphics window

lm(x~y,data=dataset)                      #basic linear model where x and y can be matrices  (see plot.lm for plotting options)

t.test(x,g)

pairwise.t.test(x,g)

power.anova.test(groups = NULL, n = NULL, between.var = NULL,
                 within.var = NULL, sig.level = 0.05, power = NULL)

power.t.test(n = NULL, delta = NULL, sd = 1, sig.level = 0.05,
             power = NULL, type = c("two.sample", "one.sample", "paired"),
             alternative = c("two.sided", "one.sided"),strict = FALSE)

## More statistics: Regression, the linear model, factor analysis and principal components analysis (PCA)
### matrices

t(X)                                     #transpose of X

X %*% Y                                  #matrix multiply X by Y 

solve(A)                                 #inverse of A

solve(A,B)                               #inverse of A * B    (may be used for linear regression)

data frames are needed for regression 

lm(Y~X1+X2)

lm(Y~X|W)            

factanal()    (see also fa in the psych package)

princomp()     (see principal in the psych package)

## Useful additional commands

colSums (x, na.rm = FALSE, dims = 1)

rowSums (x, na.rm = FALSE, dims = 1)

colMeans(x, na.rm = FALSE, dims = 1)

rowMeans(x, na.rm = FALSE, dims = 1)

rowsum(x, group, reorder = TRUE, ...)         #finds row sums for each level of a grouping variable

apply(X, MARGIN, FUN, ...)                    #applies the function (FUN) to either rows (1) or columns (2) on object X

apply(x,1,min)                             #finds the minimum for each row

apply(x,2,max)                            #finds the maximum for each column

col.max(x)                                   #another way to find which column has the maximum value for each row 

which.min(x)

which.max(x)

z=apply(x,1,which.min)               #tells the row with the minimum value for every column

# Graphics

par(mfrow=c(nrow,mcol))                   #number of rows and columns to graph

par(ask=TRUE)                             #ask for user input before drawing a new graph

par(omi=c(0,0,1,0) )                      #set the size of the outer margins 

mtext("some global title",3,outer=TRUE,line=1,cex=1.5)    #note that we seem to need to add the global title last
                     #cex = character expansion factor 
boxplot(x,main="title")                  #boxplot (box and whiskers)

title( "some title")                          #add a title to the first graph

hist()                                   #histogram

plot()
	plot(x,y,xlim=range(-1,1),ylim=range(-1,1),main=title)

	par(mfrow=c(1,1))     #change the graph window back to one figure

	symb=c(19,25,3,23)

	colors=c("black","red","green","blue")

	charact=c("S","T","N","H")

	plot(PA,NAF,pch=symb[group],col=colors[group],bg=colors[condit],cex=1.5,main="Postive vs. Negative Affect by Film condition")

	points(mPA,mNA,pch=symb[condit],cex=4.5,col=colors[condit],bg=colors[condit])

curve()
abline(a,b)
	 abline(a, b, untf = FALSE, ...)
     abline(h=, untf = FALSE, ...)
     abline(v=, untf = FALSE, ...)
     abline(coef=, untf = FALSE, ...)
     abline(reg=, untf = FALSE, ...)

identify()
	plot(eatar,eanta,xlim=range(-1,1),ylim=range(-1,1),main=title)
	identify(eatar,eanta,labels=labels(energysR[,1])  )       #dynamically puts names on the plots
locate()

legend()

pairs()                                  #SPLOM (scatter plot Matrix)

pairs.panels ()    #SPLOM on lower off diagonal, histograms on diagonal, correlations on diagonal
                   #not standard R, but in the psych package

matplot ()

biplot ())

plot(table(x))                           #plot the frequencies of levels in x

x= recordPlot()                     #save the current plot device output in the object x

replayPlot(x)                       #replot object x

dev.control                         #various control functions for printing/saving graphic files

pdf(height=6, width=6)              #create a pdf file for output

dev.of()                            #close the pdf file created with pdf 

layout(mat)                         #specify where multiple graphs go on the page
                                    #experiment with the magic code from Paul Murrell to do fancy graphic location
layout(rbind(c(1, 1, 2, 2, 3, 3),
             c(0, 4, 4, 5, 5, 0)))   

for (i in 1:5) {

  plot(i, type="n")

  text(1, i, paste("Plot", i), cex=4)

}

## Working with Dates

date <-strptime(as.character(date), "%m/%d/%y")   #change the date field to a internal form for time  
                                                  #see ?formats and ?POSIXlt  
 
as.Date

month= months(date)                #see also weekdays

dt2 <- as.Date("04/20/2011", format = "%m/%d/%Y")

dt2

## [1] "2011-04-20"

dt3 <- as.Date("October 6, 2010", format = "%B %d, %Y")

dt3

## [1] "2010-10-06"

dates <- c("05/27/84", "07/07/05")
betterDates <- as.Date(dates,
  format = "%m/%d/%y")
> betterDates
[1] "1984-05-27" "2005-07-07"

## Sync GitHub repository with existing R project

### Step 1: create a GitHub repository

Easy. Go to your github account and click the button to create a new repo. I typically do not initialize with the .gitignore, readme.md, or license.md files, but add them myself manually after the project is up and running.

### Step 2: enable git in Rstudio

Open your project in Rstudio and navigate to Tools -> Version Control -> Project Setup

Click SVN/Git tab and select git as the version control system. It will ask you to initialize a new git repo and restart Rstudio

After Rstudio reopens, confirm that there is a Git tab in the environment pane (which for me, and I think by default, is in the upper right of the IDE)

### Step 3: synchronize with the github repo

#### move to the project directory
cd Projects/website

#### initiate the upstream tracking of the project on the GitHub repo
git remote add origin https://github.com/hansenjohnson/website.git

#### pull all files from the GitHub repo (typically just readme, license, gitignore)
git pull origin master

#### set up GitHub repo to track changes on local machine
git push -u origin master

### Step 4: push files to GitHub

Click the Git tab in Rstudio, and then click Commit. This will open a window where you can stage files to be tracked (and synced on GitHub). Select all the files you would like to track, write a commit message, then click push. This will send all changes to the GitHub repo.

#SQL

## Selecting ids of all R files:

SELECT *
FROM [bigquery-public-data:github_repos.files]
WHERE lower(RIGHT(path, 2)) = '.r

## Selecting content of R files:
SELECT *
FROM [bigquery-public-data:github_repos.contents]
WHERE id IN (select id from [bigquery-github-1383:Github.r_files])

### How to create a list in R programming?
#### List can be created using the list() function.

> x <- list("a" = 2.5, "b" = TRUE, "c" = 1:3)
Here, we create a list x, of three components with data types double, logical and integer vector respectively.
### Its structure can be examined with the str() function.

> str(x)

#### However, tags are optional. We can create the same list without the tags as follows. In such scenario, numeric indices are used by default.

> x <- list(2.5,TRUE,1:3)

#### How to access components of a list?
Lists can be accessed in similar fashion to vectors. Integer, logical or character vectors can be used for indexing. Let us consider a list as follows.

> x
$name
[1] "John"
$age
[1] 19
$speaks
[1] "English" "French" 
> x[c(1:2)]    # index using integer vector
$name
[1] "John"
$age
[1] 19
> x[-2]        # using negative integer to exclude second component
$name
[1] "John"
$speaks
[1] "English" "French" 
> x[c(T,F,F)]  # index using logical vector
$name
[1] "John"
> x[c("age","speaks")]    # index using character vector
$age
[1] 19
$speaks
[1] "English" "French" 

#### Indexing with [ as shown above will give us sublist not the content inside the component. To retrieve the content, we need to use [[.

However, this approach will allow us to access only a single component at a time.

> x["age"]
$age
[1] 19
> typeof(x["age"])    # single [ returns a list
[1] "list"
> x[["age"]]    # double [[ returns the content
[1] 19
> typeof(x[["age"]])
[1] "double"

#### How to modify a list in R?
We can change components of a list through reassignment. We can choose any of the component accessing techniques discussed above to modify it.

Notice below that modification causes reordering of components.

> x[["name"]] <- "Clair"; x
$age
[1] 19
$speaks
[1] "English" "French" 
$name
[1] "Clair"

#### How to add components to a list?
Adding new components is easy. We simply assign values using new tags and it will pop into action.

> x[["married"]] <- FALSE
> x
$age
[1] 19
$speaks
[1] "English" "French" 
$name
[1] "Clair"
$married
[1] FALSE