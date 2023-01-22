# DATA ANALYSIS THE DATA.TABLE WAY <https://documentcloud.adobe.com/gsuiteintegration/index.html?state=%7B%22ids%22%3A%5B%221QVS7Av3qSRVGvHduptmpSppZNmv6Bi6C%22%5D%2C%22action%22%3A%22open%22%2C%22userId%22%3A%22103108935584454039053%22%2C%22resourceKeys%22%3A%7B%7D%7D>
library(data.table)

# General form: DT[i, j, by] - “Take DT, subset rows using i, then calculate j grouped by by” 

##### 1. Create a  data.table and call it DT. #####
set.seed(45L)
DT <- data.table(V1 = c(1L, 2L),
                 V2 = LETTERS[1:3],
                 V3 = round(rnorm(4), 4),
                 V4 = 1:12
                 )

##### 2. Subsetting rows using 'i' #####

#### 1) What?
# Subsetting rows by numbers.

### Example
DT[3:5, ] #or DT[3:5]

### Notes
# Selects third to fifth row.
#___________________________________

#### 2) What?
# Use column names to select rows in i based on a condition using fast automatic indexing. 
# Or for selecting on multiple values: 
#   DT[column %in% c("value1","value2")],
# which selects all rows that have value1 or  value2 in column.

### Example
DT[V2 == "A"] # DT[, V2 == "A"] returns a vector
DT[V2 %in% c("A", "C")]

### Notes
# Selects all rows that have value A in column V2.
# Select all rows that have the value A or C in column V2. 


##### 3. Manipulating on columns in 'j' #####

#### 1) What?
# Select 1 column in j.

### Example
DT[, V2] # or DT[, 2]

### Notes
# Column V2 is returned as a vector. 
#___________________________________

#### 2) What?
# Select several columns in j.

### Example
DT[, .(V2, V3)]
DT[, .(V2)]

### Notes
# Columns V2 and V3 are  returned as a data.table. 
#___________________________________

# ! .() is an alias to list(). If .() is used, the returned value is a data.table. If .() is not used, the result is a vector. !#
DT[, .(V2)] 
DT[, V2]
#___________________________________

#### 3) What?
# Call functions in j.

### Example
DT[, (sum(V1))]
DT[, .(sum(V1))]

### Notes
# Returns the sum of all elements of column V1 in a vector. 
# Returns the sum of all elements of column V1 in DataTable. 
#___________________________________


#### 4) What?
# Computing on several columns.

### Example
DT[, .(sum(V1), sd(V3))]

### Notes
# Returns the sum of all  elements of column V1 in a vector.
#___________________________________


#### 5) What?
# Assigning column names to  computed columns. 

### Example
DT[, .(Aggragate = sum(V1), Sd.V3 = sd(V3))]
### Notes
# The same as above, but with new names. 
#___________________________________




#### 6) What?
# Columns get recycled if different length. 

### Example
DT[, .(V1, Sd.V3 = sd(V3))]

### Notes
# Selects column V1, and compute std. dev. of V3, which returns a single value and gets recycled. 
#___________________________________


#### 7) What?
# Multiple expressions can be wrapped in curly braces. 

### Example
DT[, {print(V2)
  plot(V3)
  NULL}] 
### Notes
# Print column V2 and plot V3. 
#___________________________________
#___________________________________
#___________________________________


##### 4. Doing 'J' By Group #####

#### 1) What?
# Doing j by group.

### Example
DT[, .(V4.Sum = sum(V4)), by = V1]
DT[, .(V4.Sum = sum(V4)), by = V2]

### Notes
# Calculates the sum of V4, for every group in V1. 
# Calculates the sum of V4, for every group in V2. 
#___________________________________


#### 2) What?
# Doing j by several groups using .().

### Example
DT[ , .(V4.Sum = sum(V4)), by = .(V1, V2)]

### Notes
# The same as above, but for every group in V1 and V2. 
#___________________________________


#### 3) What?
# Call functions in by. 

### Example
DT[ , .(V4.Sum = sum(V4)), by = sign(V1 - 1)] # DT[ , .(V4.Sum = sum(V4)), by = V1]

### Notes
# Calculates the sum of V4, for every group in sign(V1 - 1). 
#___________________________________


#### 4) What?
# Assigning new column  names in by. 

### Example
DT[ , .(V4.Sum = sum(V4)), by = .(V1.01 = sign(V1 - 1))]

### Notes
# Same as above, but with a new name for the variable we are grouping by. 
#___________________________________


#### 5) What?
# Grouping only on a subset by specifying i. 

### Example
DT[1:5, .(V4.Sum = sum(V4)), by = V1]

### Notes
# Calculates the sum of V4, for every group in V1, after subsetting on the first five rows. 
#___________________________________


#### 5) What?
# Using .N to get the total  number of observations of each group.

### Example
DT[ , .N, by = V1]
### Notes
# Count the number of rows for every group in V1. 
#___________________________________


##### 5. Adding/Updating columns by reference in 'J'  using := #####

#### 1) What?
# Adding/updating a column by reference using := in one line. Watch out: extra assignment (DT <- DT[...]) is redundant.

### Example
DT[ , V1 := round(exp(V1), 2)]

### Notes
#  Column V1 is updated by what is after :=.

### Output
# Returns the result invisibly. Column V1 went from: [1] 1 2 1 2 … to [1] 2.72 7.39 2.72 7.39 … 
#___________________________________

#### 2) What?
# Adding/updating several  columns by reference using :=.

### Example
DT[, c("V1","V2") := list
  (round(exp(V1),2), LETTERS
     [4:6])]
### Notes
#  Column V1 and V2 are updated by what is after :=. 

### Output
# Returns the result invisibly.  Column V1 changed as above. Column V2 went from: [1] "A" "B" "C" "A" "B" "C" … to: [1] "D" "E" "F" "D" "E" "F" …  
#___________________________________


#### 3) What?
# Using functional :=. 

### Example
DT[, ':=' (V1 = 
           round(exp(V1),2),
           V2 = LETTERS[4:6])][]

### Notes
#  Another way to write the same line as above this one, but easier to write  comments side-by-side.
# Also, when [] is added  the result is printed to the screen. 
#___________________________________


#### 4) What?
# Remove a column instantly  using :=. 

### Example
DT[, V1 := NULL]

### Notes
# DT[, V1 := NULL] 
#___________________________________


#### 5) What?
# Remove several columns  instantly using :=. 

### Example
DT[, c("V1","V2") := NULL]

### Notes
# Removes columns V1 and V2.
#___________________________________


#### 6) What?
# Wrap the name of a variable which contains column names in parenthesis to pass the contents of that variable to be deleted.

### Example
DT[, Cols.chosen := NULL] 
DT[, Cols.chosen := NULL] 
DT[, Cols.chosen := NULL] 

### Notes
# Watch out: this deletes the column with column name Cols.chosen. 
# Watch out: this deletes the column with column name Cols.chosen.
#___________________________________



##### 6. Indexing and keys #####

#### 1) What?
# Use setkey() to set a key on a DT. The data is sorted on the column we specified by reference. 

### Example
setkey(DT,V2)
### Notes
# A key is set on column V2. 
#___________________________________

#### 2) What?
# Use keys like supercharged rownames to select rows.

### Example
DT["A"]
DT[c("A", "C")]

### Notes
# Returns all the rows where the key column (set to  column V2 in the line above) has the value A. 
# Returns all the rows where the key column (V2) has the value A or C.
#___________________________________

#### 3) What?
# Returns all the rows where the key column (V2) has the value A or C.

### Example
DT["A", mult ="first"]
DT["A", mult ="last"]
### Notes
# Returns first row of all rows that match the value A in the key column (V2). 
# Returns last row of all rows that match the value A in the key column (V2). 
#___________________________________

#### 4) What?
# The nomatch argument is used to  control what happens when a value specified in i has no match in the rows of the DT.
# Default is NA, but can be changed to 0.  0 means no rows will be  returned for that non-matched row of i. 

### Example
DT[c("A","D")]
DT[c("A","D"), nomatch = 0]
### Notes
# Returns all the rows where the key column (V2) has the value A or D. Value D is not found and not returned because of the  nomatch argument. 
#___________________________________

#### 5) What?
# by=.EACHI allows to group by each subset of known groups in i. A key needs to be set to use by=.EACHI. 

### Example
DT[c("A","C"), sum(V4)]
DT[c("A","C"), sum(V4), by=.EACHI]
library(dplyr)
DT %>% group_by(V2) %>% summarise(sum(V4))

### Notes
# Returns one total sum of column V4, for the rows of the key column (V2) that have values A or C. 
# Returns one sum of column V4 for the rows of column V2 that have value A, and  another sum for the rows of column V2 that have value C. 
#___________________________________

#### 6) What?
# Any number of columns can be set as key using setkey(). This way rows can be selected on 2 keys which is an equijoin.  

### Example
setkey(DT,V1,V2)
DT[.(2,"C")]
DT[.(2, c("A","C"))]
### Notes
# Sorts by column V1 and then by column V2 within each group of column V1. 
# Selects the rows that have the value 2 for the first key (column V1) and the value C for the second key (column V2). 
# Selects the rows that have the value 2 for the first key (column V1) and within those rows the value A or C for the second key (column V2). 
#___________________________________

##### 7. Advanced Data Table operations #####
# 1) .N contains the number of rows or the last row. 
DT[.N - 1] # Usable in i:
DT[.N]
DT[,.N] # Usable in j: 
# Returns the penultimate row of the  data.table. 
# Returns the number of rows. 
#___________________________________

# 2) .() is an alias to list() and means the same. The .() notation is not  needed when there is only one item in by or j.
DT[,.(V2,V3)] #or  DT[,list(V2,V3)]  # Usable in j:
DT[, mean(V3), by =.(V1,V2)] # Usable in by: 
# Columns V2 and V3 are returned as a data.table. 
# Returns the result of j, grouped by all possible combinations of groups specified in by. 
#___________________________________

# 3) .SD is a data.table and holds all the values of all columns, except the one specified in by. 
#     It reduces  programming time but keeps  readability. .SD is only accessible in j. 
DT[, print(.SD), by = V2]
DT[,.SD[c(1,.N)], by = V2]
DT[, lapply(.SD, sum), by = V2]

DT %>% group_by(V2) %>% summarise(sum(V1), sum(V3), sum(V4))

# To look at what .SD  contains. 
# Selects the first and last row grouped by column V2.
# Calculates the sum of all columns in .SD grouped by V2.
#___________________________________

# 3) .SDcols is used together with .SD, to specify a subset of the columns of .SD to be used in j.
DT[, lapply(.SD,sum), by=V2, .SDcols = c("V3","V4")]
# Same as above, but only for columns V3 and V4 of .SD. 
#___________________________________

# 4) .SDcols can be the result of a  function call. 
DT[, lapply(.SD,sum), by=V2, .SDcols = paste0("V",3:4)]
# Same result as the line above.  
#___________________________________
#___________________________________


##### 8. Chaining help tack expression together and avoid (unnecessary) intermediate assignments #####
# 1) Do 2 (or more) sets of statements at once by chaining them in one statement. 
#    This  corresponds to having in SQL. 
DT <- DT[, .(V4.Sum = sum(V4)),by=V1] 
DT[V4.Sum > 40] #no chaining
DT[, .(V4.Sum = sum(V4)), by=V1][V4.Sum > 40 ]
# First calculates sum of V4, grouped by V1. Then selects that group of which the sum is > 40  without chaining. 
# Same as above, but with chaining.
#___________________________________

# 2) Order the results by chaining. 
DT[, .(V4.Sum = sum(V4)), by=V1][order(-V1)]
# Calculates sum of V4, grouped by V1, and then orders the result on V1. 
#___________________________________

##### 9. Using the  "set" family #####
# 1) set() is used to repeatedly  update rows and columns by reference. Set() is a loopable low overhead 
#    version of :=. Watch out: It can not handle grouping operations.  
Syntax of  set(): for (i in from:to)
set(DT, row, column, new value).  
rows = list(3:4,5:6) 
cols = 1:2  
for (i in seq_along(rows)) 
{ set(DT,
        i=rows[[i]],
        j = cols[i],
        value = NA) } 
# Sequence along the values of rows, and for the values of cols, set the  values of those elements equal to NA.
#___________________________________

# 2) setnames() is used to create or update column names by  reference. 
setnames(DT,"old","new")[]
# Changes (set) the name of column old to new. Also, when [] is added at the end of any set() function  the result is printed to the screen. 

setnames(DT,"V2","Rating") 
setnames(DT,"V2","Rating")[]
# Sets the name of column V2 to Rating. 

setnames(DT,c("V2","V3"), c("V2.rating","V3.DataCamp"))
# Changes two column names. 

# 3) setcolorder() is used to  reorder columns by reference. 
setcolorder(DT, "neworder")[]
# neworder is a character vector of the new column name ordering. 
setcolorder(DT, c("V2","V1","V4","V3"))[]
# Changes the column ordering to the contents of the vector. 



#### ) What?
# 

### Example

### Notes
#
#___________________________________

DT <- data.table(V1 = c(1L, 2L),
                 V2 = LETTERS[1:3],
                 V3 = round(rnorm(4), 4),
                 V4 = 1:12
)

