library(tidyverse)
library(glue)
for (month in 1:5) {
  print(paste('Month:', month))
}

for (month in 1:5) {
  if (month < 3) {
    print(paste('Winter, month', month))
  } else {
    print(paste('Spring, month', month))
  }
}

for(month in 1:5) if(month < 3)print(paste('Winter, month', month))else print(paste('Spring, month', month))

vect_1 <- c(2, 7, 4, 9, 8)
vect_2 <- numeric()

for(num in vect_1) {
  vect_2 <- c(vect_2, num * 10)
}

vect_2

animals <- c('koala', 'cat', 'dog', 'panda')
animals

for (animal in animals) {
  print(animal)
}

my_list <- list(c(5, 8, 2, 9), 'cat', 'dog', c('koala', 'panda', 'rabbit'), TRUE, 3.14)
my_list

for (item in my_list) {
  print(item)
}

for (item in my_list) {
  print(length(item))
}

my_list_2 <- list()

for(i in 1:length(my_list)) {
  my_list_2[[i]] <- rep(my_list[[i]], 2)
}

my_list_2

my_list_3 <- list()

for(i in 1:length(my_list)) {
  if (length(my_list[[i]]) > 1) {
    my_list_3[[i]] <- rep(my_list[[i]], 2)
  } else {
    my_list_3[[i]] <- 'Too short item'
  }
}

my_list_3

my_matrix <- matrix(1:9, nrow=3, ncol=3)
my_matrix
for (row in 1:nrow(my_matrix)) {
  for (col in 1:ncol(my_matrix)) {
    print(paste('Row', row, 'col', col, 'value', my_matrix[row, col]))
  }
}

qualities <- c('funny', 'cute', 'friendly')
animals <- c('koala', 'cat', 'dog', 'panda')

for (x in qualities) {
  for (y in animals) {
    print(paste(x, y))
  }
}

super_sleepers <- data.frame(rating=1:4,
                             animal=c('koala', 'hedgehog', 'sloth', 'panda'),
                             country=c('Australia', 'Italy', 'Peru', 'China'),
                             avg_sleep_hours=c(21, 18, 17, 10))

print(super_sleepers)

for (row in 1:nrow(super_sleepers)) {
  for (col in 1:ncol(super_sleepers)) {
    print(paste('Row', row, 'col', col, 'value', super_sleepers[row, col]))
  }
}
