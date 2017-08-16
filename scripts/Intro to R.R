##############
# Before you begin:
# 1. Make sure you have internet access to install packages
##############

##############
# PART 1
# R as calculator
##############

# 1.1 Assignment operators
x <- "Hello world"
x
x = 4
x
5 -> y
y
t = TRUE
f = FALSE

# 1.2 Arithmetical operators
z=x+y
z
x*y    # We will not bother storing the result to a variable any more
x-y
x/y
x^y    # x to the power of y

# 1.3 Logical operators
t & f
t | f
!t

# Compare ||,&& with |,&
# You will normally want to use | and &
v1=c(TRUE,FALSE) # c conjoins items into a vector
v2=c(FALSE,FALSE)
v1&v2
v1&&v2
v1|v2
v1||v2

# 1.4 Comparison operators
x<y
x>y
x<=y
z=x
x>=z
y==z
y!=z

# 1.5 min and max
v=c(1,4,7,3,5,9)
min(v)
max(v)

##############
# PART 2
# Vectors
##############
# Vectors are 1-dimensional ordered collections of items,
# all of which must be the same basic type

# 2.1 Accessing by indices and []
v=c(1,4,7,3,5,9)
v[1]
v[4]
w = c(1,3,5)
v[w]
v[c(1,3,5)] # No need to use w
v[-2]

# 2.2 The : operator
1:3 # Creates a vector of integers: 1,2,3
v[2:4]
w=v[2:4]
w[2]
v[2:4][2] # No need to use w

# 2.3 Vectorized operations.
# Operations normally 'vectorize'. This means they are applied to each element
# in a vector
w=v-1
# When two vectors of the same length are given, the operation will be
# applied pairwise to each pair of elements
w*v
# Look at 1.3 for logical operations
# Note that |,& vectorize, but || and && do not

# 2.4 c
# We know c can create vectors from scalars
# It also joins multiple vectors together
# (actually scalars are just vectors of length one)
c(v,w)
c(v,v,v,w,v)

# 2.5 The sample function
# The sample function samples randomly from a vector.
v=1:10
# If you only pass the vector to sample from, it just rearranges
# the items of the vector
# (It samples all elements without replacement)
sample(v)
# If you pass the vector and a number, it samples that many items
# without replacement (each item can only be sampled once)
sample(v,5)
# If you want to sample with replacement, so that each item can only be
# sampled more than once, you need to specify TRUE in the third argument
sample(v,20,TRUE)
sample(v,20,FALSE) # This will fail!
# Normally each item of the vector has the same chance of being sampled.
# You can weight the samplings by giving a vector of weights
sample(v,20,TRUE,10:1)

# 2.6 Exercises
v=sample(1:10,20,TRUE)

# We can find the length of a vector:
length(v)

# The mean, sum, prod and sd function do what you would expect:
# They give the mean, sum, product and standard deviation of the vector
mean(v)
sum(v)
prod(v)
sd(v)

# If you use a comparison operator you get a vector of logical
# values that show which items compared TRUE.
w = v>=5
w
# The which function tells us which items of a logical vector are TRUE
which(w)
# We can compress this:
which(v>=5)

# The above gives us the indices of items bigger then or equal to 5 in v.
# If we want the values we can do:
w=which(v>=5)
v[w]
# Or we can compress it to:
v[which(v>=5)]

# Multiple comparisons take advantage of the fact that logical operators
# vectorize:
which(v>8|v<3)     # indices
v[which(v>8|v<3)]  # values

##############
# PART 3
# Matrices
##############
# Matrices are 2-dimensional ordered collections of items,
# all of which must be the same basic type

# We will not use matrices much.

# You can create them using the matrix function
# This needs a vector and a valid specification of the dimensions
# Since the vector is of a particular length, you only need to specify
# the number of rows or the number of columns:
matrix(1:8,nrow=4)
matrix(1:8,ncol=4)

# By default the matrix is filled in column-wise
# You can fill it in row-wise too:
matrix(1:8,ncol=4,byrow=TRUE)

# You can also bind vectors together to form a matrix.
# Either by row:
v=1:10
w=101:110
rbind(v,w)

# Or by column
cbind(v,w)

# You can use these to bind appropriate sized matrices together by row or
# column too
m=matrix(1:8,ncol=4,byrow=TRUE)
n=matrix(101:108,ncol=4,byrow=TRUE)
rbind(m,n)

# You can access items and sub-matrices using [,]
m[2,2] # Get the item at the second row, second column
m[2,2:4] # Get the items at the second row, columns two to four
m[1,-2] # Get the items at the first row, all columns except two
m[,c(1,4)] # Get the whole first and fourth columns
m[1,] # Get the whole first row

# You can ask about the number of rows and columns of a matrix, as well as the
# length of the underlying vector (the total number of items)
nrow(m)
ncol(m)
length(m)

# You can assign names to the columns and rows of a matrix
colnames(m)=c("A","B","C","D")
rownames(m)=c("a","b")
m

# You can see these names by:
colnames(m)
rownames(m)

##############
# PART 4
# Lists
##############
# Lists are 1-dimensional ordered collections of items,
# each of which can be any type, including other collections

# We will not create lists much
# But most objects in R are lists so it is useful to understand them a little.

# We create a list using the list function:
m=list(1,"Hello",TRUE,c(11,12))
m

# We can access sub-lists using []
m[1:2]    # Get a list containing the first two items
m[c(2,4)] # Get a list containing the second and fourth items
m[-2]     # Get a list containing all items except the second
m[2]      # Get a list containing only the second item

# The last command does not return the second item.
# It returns a list, with a single item which is the second item of m.
# To get the item itself we must use [[]]
m[[2]]    # Get the second item of list m

# We can name the items/fields of a list
n=list(A=1,B="Hello",C=TRUE,D=c(11,12))
n

# Now we can access the items in the list by their names using $
n$B

# We can combine lists using c or append
c(m,n)
append(n,m)

##############
# PART 5
# Data frames
##############
# Data frames are R's most common representation of datasets.
# Each columns must contain items of all the same basic type.
# But different columns can contain items of different types.

# We will use them a lot!

# We will use a data set from the car package.
# To install and load car type:
install.packages("car")
library(car)

# The data function can load a dataset as a data frame from a loaded package
data(Duncan)

# head shows us the first six rows. Useful for a quick view of what sort
# of data we are dealing with
head(Duncan)

# summary gives us some summary statistics for each column. Also useful for
# a first understanding of our data
summary(Duncan)

# Accessing items, columns, rows and sub-data frames
# For the 1D [] operator, data frames work like lists of vectors.
# So we can access sub-data frame consisting of particular columns using []
Duncan[1]        # Get a df consisting of the first column
Duncan[c(1,3,4)] # Get a df consisting of the first, third and fourth columns
Duncan[-3]       # Get a df consisting of all columns except the third

# For the 2D [,] operator, data frames work like matrices.
# So we can access items, sets of rows or columns, sub-data frame using [,]
Duncan[11,2]     # Get the item on row 11, column 2
Duncan[,2]       # Get all items in column 2 (as a vector)
Duncan[-12,2]    # Get all items in column 2 (as a vector) except that on row 12
Duncan[20,]      # Get all items in row 20 (as a single row dataframe)
Duncan[11:15,2:3]# Get a data frame consisting of rows 11-15 and columns 2-3
# etc

# Columns and rows can have names
# You can view them and edit them justlike we saw for matrices.
colnames(Duncan)
# You can pick individual columns (as a vector) using their name and $
Duncan$prestige

##############
# PART 6
# Functions in R
##############
# Functions are the core of programming.
# You pass them arguments, they perform computations based on these
# arguments and return the results.

# In R, functions can be stored as any other variable.
# They are defined using 'function':
myFunction=function(first,last,step) {
  # The seq function just creates a sequence of numbers
  # from first to last in step increments
  seq(first,last,step)
}
myFunction(0,10,1)
# Note implicit return: The result of the last command is returned

# Explicit return is possible:
myFunction=function(first,last,step) {
  if (first<0)
    return ("Invalid value for first.")
  seq(first,last,step)
}

# When we can a function, the arguments can be identified either by order
# or by name.
myFunction(0,10,1)
myFunction(last=10,step=1,first=0)
# Or by a combination, where order is used until the first named argument
myFunction(6,step=1,last=10)

# Functions often give argument default values.
# If the calling code does not specify values for these arguments,
# they will be given the default values.
mySeq = function (first=0,last=10,step=1) {seq(first,last,step)}
mySeq(1,5,2)
mySeq(1,5)
mySeq(1)
mySeq(last=6)
mySeq(step=3)
mySeq(2,step=3)
mySeq()

##############
# PART 7
# Control
##############

# 7.1 Conditional execution
f=function(a,b) {
  if (a<b) {
    print("Less than")
  }
  else if (a>b) {
    print("Greater than")
  }
  else {
    print("Equal")
  }
}

# 7.2 Loops
# We will only use for loops and apply functions
# For loop
v=1:10
for (i in v) {
  print(i)
}
# We run the loop code one time for each value in v. i is assigned this value

##############
# PART 8
# The apply family
##############

# The apply family takes a function and applies it to each item in
# a vector or list, or row/column in a matrix
# They are much more efficient (faster) than loops

# lapply will return a list of the results
v=1:10
lapply(v,function(x){x-1})
lapply(v,function(x){c(x,2*x)})

# sapply will try and return a vector or a matrix:
sapply(v,function(x){x-1})
sapply(v,function(x){c(x,2*x)})
# If the function returns different typed values for different items, sapply
# will be forced to return a list

# apply takes a matrix and applies the function to each row, column or item
# The second argument indicates which of these are to be done:
# 1: rows, 2: columns
m=matrix(1:8,nrow=2)
apply(m,1,sum)
apply(m,2,sum)

# In all cases, if the function involved takes more arguments, these
# are passed after the function.
f=function(x,y) {x+y}
sapply(v,f,10)
sapply(v,f,1000)

##############
# PART 9
# 2d plotting
##############

# We have already loaded the Duncan data above, so we can continue using it.
# To load it:
# library(car)
# data(Duncan)

# Exercises
# Plotting data points is easy:
plot(Duncan$education,Duncan$prestige)
# To give the axes better names, try:
plot(Duncan$education,Duncan$prestige,xlab="Education",ylab="Prestige")
# Or:
plot(Duncan[3:4])
# You can add a title using main:
plot(Duncan[3:4],main="Income vs Prestige of Careers")


# We can make the function
f=function(x) {20-.75*x+.015*x^2}
# We can plot any continuous function in two ways.
# Firstly, we can use plot:
plot(f,from=0,to=100,col="blue",add=TRUE)
# This uses the f function to calculate a number of points between 0 and 100
# and joins them with blue lines. If we do not specify add="TRUE" it will
# draw a new plot rather than adding it to the current plot.
# Alternatively, we can use points:
plot(Duncan$education,Duncan$prestige,xlab="Education",ylab="Prestige")
xseq=0:100
points(xseq,f(xseq),type="l",col="blue")
# Normally points just adds points to the current plot. But because we specify
# type="l" connects each pair of points with blue lines.

# We can plot the distance from each point to our curve using:
segments(Duncan$education,Duncan$prestige,Duncan$education,f(Duncan$education),col="red")
# This gives the from and to X,Y coordinates for a set of line segments.

##############
# PART 10
# 3D plotting
##############

# This is not required for the course.
# We use the rgl package for 3d plotting. Make sure it is installed.
# If you are using a MAC you need to have X11 installed.
# This was shipped with OS X 10.5-10.7 but now needs to be manually installed.
# You can get it from: https://www.xquartz.org/

# To install and load rgl type:
install.packages("rgl")
library(rgl)

# We plot the points:
plot3d(Duncan[2:4],main="Income & Education vs Prestige of Careers")

# Here is the function we want to plot:
g=function(x,y) {
  m=cbind(x,y) # combine vectors into a matrix column-wise
  apply(m,1,max) # apply max to each row
}
# To plot a function in 3d we need to plot a surface.
# First we need to construct a grid of X and Y points
xseq=0:100
yseq=0:100
# Now we set up a matrix of all pairs...
# rep with each specified repeats each argument of xseq the given number of times
xseq_=rep(xseq,each=101)
# rep without each specified repeats yseq the given number of times
yseq_=rep(yseq,101)
# Now we need the heights for each of these points
heights=g(xseq_,yseq_)
# And we can use the surface3d function to plot the surface
surface3d(xseq,yseq,heights,col="blue",alpha=.7) # alpha makes it transparent

# Now we plot the distance from the points to the surface.
# segments3d takes a matrix or dataframe where each odd row is the
# from coordinate and the the following row the to coordinate for a
# line segment.
# The from coordinates are just the points: Duncan[2:4]
from=Duncan[2:4] # Not needed, we could just use Duncan[2:4] below
# The to coordinates are the original income and education and the g outputs
# for these vectors:
to=cbind(Duncan[2:3],g(Duncan$income,Duncan$education))
# Merging them is easiest using the interleave function from gdata
# install gdata if you need to
library(gdata)
# But we need the column names to match, so:
colnames(to)=colnames(from)
m=interleave(from,to)
# Now we are ready
segments3d(m,col="red")
# You can zoom in and out (hold right mouse button down and go up/down)
# and rotate (hold left mouse button down and go left/right/up/down).
# Play around.
