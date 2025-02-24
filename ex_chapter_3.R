#----Gauss Variations----

#with for loop
vec <- c(1:100)
vec_sum <- 0

for (i in seq(length(vec))){
  vec_sum <- vec_sum + vec[i]
}

vec_sum


#with while loop

vec <- c(1:100)
vec_sum <- 0
i = 1

while (i <= 100){
  vec_sum <- vec_sum + vec[i]
  i = i + 1
}

vec_sum


vec <- c(1:100)
vec_sum <- 0
for (i in seq(length(vec))){
  if (vec[i] %% 3 == 0 & vec[i] %% 7 == 0) {
    vec_sum <- vec_sum + vec[i]
  }
}

cat("The sum of multiples of 3 and 7 between 1 and 100 is:", vec_sum)


#----Interpolation----
vec <- c(rep(NA, 100))
  for (i in c(1:100)){
    if (i <= 25){
    vec[i] <- 6
  }
    else if (i >= 66){
      vec[i] <- -20
  }
}
plot(c(1:100), vec)
plot(approx(vec, method = "linear"))


#----Nested Loops----
mymat <- matrix(c(6, 7, 3, NA, 15, 6, 7, 
                  NA, 9, 12, 6, 11, NA, 3, 
                  9, 4, 7, 3, 21, NA, 6, 
                  rep(NA, 7)),
                nrow = 4, byrow = TRUE)
myvec <- c(8, 4, 12, 9, 15, 6)

for (i in seq(4)){
  maximum <- max(myvec)
  for (j in seq(length(mymat[i,]))){
    if (is.na(mymat[i,j])){
      mymat[i, j] <- maximum
    }
  }
  myvec <- myvec[!(myvec == maximum)]
}
print(mymat)



