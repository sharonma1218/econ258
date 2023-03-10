
#-------------------------------------#
# ECON 258                            #
#                                     #
# Lesson 1: Introduction to R         #
#                                     #      
# Ardina Hasanbasri                   #
# ardinah@umich.edu                   #
#                                     #
#-------------------------------------# 

##### PART 1: Setting Up #####

  # Set working directory  

  getwd() # shows you which directory you are currently in 
  setwd("C:/Users/ardin/OneDrive/Desktop/Econ 258 Winter 2021")
  
  setwd("~/Desktop/Econ 258 R/Lesson 1 Code") 
        # easiest way to set directory: session > set working directory > to source file location
  setwd("C:/Users/sharonma0975/Desktop/Econ 258 R/Lesson 1 Code")

  # Install and load packages 

  install.packages("mosaic") # Only need to be installed once

  library(mosaic)

##### PART 2: Simple Objects and Vectors ######

  # Creating objects 
  
  x <- 2 
        # "<-" means assign. option & (-) on keyboard automatically creates. 
  y <- 3
  
  z <- x + y 
  w <- x - y 
  
  # Creating a vector  
    
  vector <-c(1,3,5)
  
  # Note 1: newly created objects show up in the environment window
  # Note 2: R is character and case sensitive, typing vecto in the console will produce an error
  
  # The symbols . and _ can also be used in naming an object
  v.1 <- c(x, y, z, w, 1, 9)
  v.2 <- c(v.1, v.1)
  
  # Let's sort v.2 
  a <- sort(v.2) # assigning ensures that the sorting has changed 
  a 
  
  # Find the length of vector
  length(v.2)
  
  # Extracting elements 
  # useful to see parts of your data; e.g. just women's salaries
  vector[3] # the 3rd element of the vector 
  v.2[length(v.2)] 
        #"length(v.2)" is the total number of elements in v.2
        #so "v.2[length(v.2)" is the last element of the vector]
        #similarly, "v.2[3]" is the 3rd element of the vector 
  
  ############################################################# number of rows ?
  v.2[1:3] # all the elements from 1 to 3 

  # A negative value, deletes the element
  vector
  vector[-2]
  vector[-c(1:3)]
  
  # A few vector manipulations that we can do 
  vector*2
  vector*vector
  sum(vector)
  prod(vector)
  mean <- sum(vector)/length(vector)
  mean
  
  # vector can also be string values 
  string_vector <-c('apple', 'grape', 'orange')
        # without the apostrophes, R will think that "apple" is an object and 
        # try to look for it in the environment when it's not there 
  string_vector[3]
  
##### PART 3: Matrices ######

  # Now let's move on to creating matrices 
  # Here is a simple 2 by 2 matrix
  M <- matrix(data=c(10,5,0.5,1), nrow=2, ncol=2)
  M 
  # when you do help("matrix"), you will notice that "byrow = FALSE" by default.
  # you can add ", byrow = TRUE" to sort by row. (exercise q2)
  
  P1 <-matrix(input <- c(1:10),nrow=2,ncol=5) # Observe the difference between have = and <- 
  P2 <-matrix(1:10, nrow=2, ncol=5, byrow=TRUE)
  
  # combining multiple vectors into a matrix 
  a <- c(1,2)
  b <- c(3,4)  
  c <- c(5,6)
  
  cbind(a,b,c) # letting each vector have in its own column
  rbind(a,b,c) # letting each vector have its own row 
  
  #checking the dimension of your matrix
  dim(cbind(a,b,c))
  
  # Extracting elements from a matrix A[row, column]
  B <- matrix(1:21, nrow=3, ncol=7)
  
  B[2,3] # second row, third column 
  B[ ,3] # third column
  B[2, ] # second row 
  
  # You can rewrite particular elements 
  B # see before
  B[1,1]<-1000
  B # see after
  B[c(2,3),2]<-2000
  B # see changes again 
  
  # A few matrix operations and algebra
  C <- matrix(1:9, nrow=3, ncol=3)
  t(C) # transpose (flipping a matrix by switching its rows with its columns)
 
  D <- diag(3) #identity matrix
  D*2
  D*3 + D  

  # For matrix multiplication cannot use * symbol, instead use %*% 
  M1 <- rbind(c(2,5,1),c(3,4,2))  #2x3 matrix
  M2 <- cbind(c(-1,1,1),c(6,1,3)) #3x2 matrix
  M1%*%M2
  dim(M1%*%M2) # "dim" means "dimension"

  # What happens if you use the * symbol instead of %*%?
  M1*M2 # This spits out an error 
  c(1,2)*c(3,4) # This multiplies element by element
  
      # Note 3: if you need help with any command you can type help(....), for example: help(matrix)
    
  
##### PART 4: Relational Operations ######
  
  # True or False statements
  
  3==2
  length(M2)==6

  ones <- rep(x=1, times=4)
  help(rep) # Observe why it is x = ..., we are not naming the element 4, but the attribute x is 1, y=1 will not work
  
  ones!=2 # "!=" means "not equal to"    

  c(1,2,3,4)>3  

  # Using AND (&), OR (|)

  length(M2)== 2|6 # is the total number of elements in M2 equal to 2 or 6? 

  c(1,2,3,4)>2 & c(1,2,3,4)<4   
  
