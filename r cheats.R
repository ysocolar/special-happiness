class(dat) #will tell you it's a dataframe
typeof(dat[,1]) #will tell you it's "integer"

#python is zero indexed (first row is 0 in python)

dat[c(1,2,6), c(7,8,9)] #will look at column spots 7, 8, 9 for rows 1,2,6 (3x3 matrix)


patient_10 <- dat[10,]
min(patient_10)
max(patient_10)
mean(as.numeric(patient_10)) # patient_10 will be of class "dataframe"

apply(dat, 1, mean) #1 is row, 2 is columns
col.means <- apply(dat, 2, mean)
col.means
class(col.means) #doesn't keep it as a dataframe, though it will keep the column names for reference

summary(dat) #will give summary of each column
summary(dat[4:6, 1]) #will give one summary of patients 4, 5, 6, column 1

#plotting
row.max <- apply(dat, 1, max)
col.sd <- apply(dat,2,sd)
plot(row.max)
plot(row.max, type = "l") #connects datapoints with a line
plot(col.sd, type = "l")

mean_1to10 <- apply(dat[1:10,], 2, mean)
plot(mean_1to10, type = "l", ylab = "mean", xlab = "day")


#writing functions
far_to_kelvin <- function(temp_F) {
  temp_K <- (temp_F -32) * (5/9) + 273.15
  return(temp_K)
}

  #or, but better to use return
far_to_kelvin <- function(temp_F) {
  (temp_F -32) * (5/9) + 273.15
}

far_to_celsius <- function(temp_F) {
  temp_C <- far_to_kelvin(temp_F) -273.15
  return(temp_C)
}

(temp_k <- far_to_kelvin(77)) #wrapping in () will return output in console as well as making assignment


all.equal(far_to_celsius(51), 10.55556) #checks to see if answers match. Will be logical if true, gives mean relative difference if false

minmax <- function(vector) {
  min <- min(vector)
  max <- max(vector)
  return(sum(min, max))
}

minmax(col.min)

far_conversion <- function(temp_F, to = "C") { #defines a default argument
  
  stopifnot(class(temp_F) == "numeric") #check class of temp_F before function starts
  stopifnot(class(to) == "character")
  
  if (to == "C") {
    temp_out <- far_to_celsius(temp_F)
  } else if (to == "K") {
    temp_out <- far_to_kelvin(temp_F)
  } else {
    message("unexpected input")
  }
  return(temp_out)
}


input_temp <- as.numeric(50:90)

far_conversion_plot <- function(temp_F, to = "C") { #defines a default argument
  
  stopifnot(class(to) == "character")
  
  if (to == "C") {
    temp_out <- far_to_celsius(temp_F)
  } else if (to == "K") {
    temp_out <- far_to_kelvin(temp_F)
  } else {
    message("unexpected input")
  }
  return(plot(input_temp, temp_out))
}

far_conversion_plot(input_temp)


analyze <- function(file_name) {
  dat <- read.csv(file = file_name, header = FALSE)
  col.max <- apply(dat, 2, max)
  col.min <- apply(dat, 2, min)
  col.mean <- apply(dat, 2, mean)
  return(rbind(col.max, col.min, col.mean))
}

analyze("data/inflammation-01.csv")

list.files() #will run in current wd
inflammation_files <- list.files("data", pattern = "inflammation", full.names = T)


inf_results <- vector("list", length(inflammation_files))
i <- 1 #alternatively you can use seq_along(inf_results)
for (file in inflammation_files) {
  print(paste("Analyzing", file)) #paste will add them both together, otherwise print won't work with comma
  out <- analyze(file)
  inf_results[[i]] <- out
  i <- i + 1
}

#or

for (file_index in seq_along(inflammation_files)) {
  file <- inflammation_files[file_index]
  print(paste("Analyzing", file)) #paste will add them both together, otherwise print won't work with comma
  out <- analyze(file)
  inf_results[[file_index]] <- out
}


# lapply take in arbitrary structure, X, and a function, FUN, and applies it over that X. Output will always be list
#lapply is much faster than a for loop
inf_results2 <- 
  lapply(X = seq_along(inf_results), FUN = function(file_index){
    file <- inflammation_files[file_index]
    print(paste("Analyzing", file)) #paste will add them both together, otherwise print won't work with comma
    out <- analyze(file)
    return(out)
  })








