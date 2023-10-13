a <- scan("pg10.txt",what="character",skip=104) ## skip contents
n <- length(a)
a <- a[-((n-2886):n)] ## strip license
a <- a[-grep("[0123456789]:[0123456789]",a)] ## strip out verse numbers

# function to split given punctuation mark from a given list of words
split_punct <- function(words, punc_mark) {
  # string concatenating white space and punctuation mark  
  space_punc <- paste("", punc_mark)
  # puts a space in between the word and the punctuation mark 
  words <- gsub(punc_mark, space_punc, words, fixed = TRUE)
  
  # list of words collapsed to a string separated by a space
  words = paste(words, collapse = " ")
  # converts collapsed string back to list of words 
  words = strsplit(words, " ")[[1]]
  
  return(words)
}

#           
a <- split_punct(a, ",")
a <- split_punct(a, ".")
a <- split_punct(a, ";")
a <- split_punct(a, "!")
a <- split_punct(a, ":")
a <- split_punct(a, "?")




a_lower<-tolower(a) # converts a to lowercase
a_unique<-unique(a_lower) # gets the unique words from a_lower


# vector of indicies indicating which element in the unique word vector each
# element in the (lower case) bible text corresponds to
index <- match(a_lower, a_unique)


# count of how many times each unique word occurs in the text
freq<-tabulate(index)

 
m = 500
threshold <- 5
# while loop to check the threshold limit such that m ~ 500
while (length(freq[freq>threshold]) >= m) {
  over_thresh <- threshold
  # increment threshold value
  threshold <- threshold + 5
}
under_thresh <- threshold

# equates threshold to a value(either threshold that makes m >= 500 or m < 500) 
# such that m is closest to 500.
threshold <- if ((length(freq[freq>over_thresh]) - m) <= (m - length(freq[freq>under_thresh]))) over_thresh else under_thresh


j <- 1
# empty vector
b <- c()
# for loop to iterate over freq vector
for (count in freq) {
  # checks if current element of freq vector is >= threshold found
  if (count >= threshold) {
    # if it is, append the word to vector b
    b = append(b, a_unique[j])
  }
  j <- j + 1
}

#     

text_index <- match(a_lower, b)


# columns for matrix
matrix_col1 <- text_index[1:(length(text_index)-2)]
matrix_col2 <- text_index[2:(length(text_index)-1)]
matrix_col3 <- text_index[3:(length(text_index))]

# bind the above vectors into desired matrix
matrix <- cbind(matrix_col1, matrix_col2, matrix_col3)


# the new matrix now only contains rows without NA. The way this was done was
# by counting how many NA's were in each row, keeping only the ones with none
matrix_new <- matrix[rowSums(is.na(matrix))==0, ]


b_n <- length(b)
# 3D array initialized to all 0s
T <- array(c(0,0), dim=c(b_n, b_n, b_n))
# loop to go through every row of matrix_new and adding 1 to the corresponding
# slot in the array
for (i in 1:nrow(matrix_new)){
  T[matrix_new[i,1],matrix_new[i,2],matrix_new[i,3]] = 
    T[matrix_new[i,1],matrix_new[i,2],matrix_new[i,3]] + 1
}


# matrix A to fill in with probabilities
A <- array(c(0,0), dim=c(b_n, b_n))
for (i in 1:nrow(matrix_new)){
  A[matrix_new[i,1],matrix_new[i,3]] = 
    A[matrix_new[i,1],matrix_new[i,3]] + 1 
}
# vector S to fill in with probabilities
S <- rep(0, b_n)
for (i in 1:nrow(matrix_new)){
  S[matrix_new[i,1]] = S[matrix_new[i,1]] + 1
}
  
#       
num_words <- 50
sim_text <- rep("", num_words)
# randomly pick a word from b, based on the probabilities in S
sim_text[1] <- sample(b, size = 1, prob = S)

# if statement used for second word as we have to fall back to S if the sample 
# word does not follow the first one. We also check if there are any non-zero 
# values
if (sum(A[match(sim_text[1], b), ] != 0) != 0 && 
     sample(b, size = 1, prob = A[match(sim_text[1], b), ]) != 0){
  sim_text[2] <- sample(b, size = 1, prob = A[match(sim_text[1], b), ])
}else sim_text[2] <- sample(b, size = 1, prob = S)

# for loop to iterate to fill in remaining words
for (i in 3:length(sim_text)){
  # check if there are more than 0 occurrences of the sampled word coming after the
  # previous one. We also check if there are any non-zero elements in the vector, if not, then
  # we fall back to A.
  if(sum(T[match(sim_text[i-2], b), match(sim_text[i-1], b), ] != 0) != 0 && 
     sample(b, size=1, prob = T[match(sim_text[i-2], b), match(sim_text[i-1], b), ]) != 0){
    sim_text[i] <- sample(b, size=1, prob = T[match(sim_text[i-2], b), match(sim_text[i-1], b), ])
  # fall back to A if word does not follow the pair b_i, b_k
  }else if (sum(A[match(sim_text[i-2], b), ] != 0) != 0 &&
            sample(b, size = 1, prob = A[match(sim_text[i-2], b), ]) != 0){
    sim_text[i] <- sample(b, size = 1, prob = A[match(sim_text[i-2], b), ])
  # fall back to S
  } else sim_text[i] <- sample(b, size = 1, prob = S)
}

#printing the text
cat(sim_text)

#             <-------------Q9------------->
# same thing as question 8 but only relying on vector S
sim_text_S <- rep("", num_words)
for (i in 1:length(sim_text)){
  sim_text_S[i] = sample(b, size = 1, prob = S)
}

# printing the text
cat(sim_text_S)


#             <-------------Q10------------->
#all the unique words, including those with capitals and without
unique_capitals<-unique(a)

#the difference to find just the capitalised words
difference<-setdiff(unique_capitals,a_unique)

#index and frequency of capitalised words
capital_index<-match(a,difference)
capital_freq<-tabulate(capital_index)

#lowercase the difference between the two vectors
lower_difference<-tolower(difference)

#how often words show up in the text in general
total_words<-match(a_lower,lower_difference)
index_total<-tabulate(total_words)

#empty vector to hold capitalised words capital_b
capital_b <- c()

#the fraction of capitalised words/all words, this will be used to find decimal of how often the word shows up
division<-(capital_freq/index_total)

k <- 1
#for every element in division
for (i in 1:length(division))
{
  #if a words shows up more then 50% of the time add it to a new vector capital_b
  if (division[i] > 0.5)
  {
    capital_b<-append(capital_b,difference[k])
  }
  k<-k+1
}

#lowercase these words
lower_b<-tolower(capital_b)

#Function to reintroduce capitals letters to the beginning of a word
UpperCase <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#Vector containing the common elements of b and lower_b
b_common<-Reduce(intersect, list(b,lower_b))

#reintroduce capital letters to words
common_capitals<-UpperCase(b_common)

sim_text_C <- rep("", num_words)
for (i in 1:length(sim_text_C))
{
  #random sample from simulation
  result<-sample(b, size =1,prob = S)
  #now we want to compare our sample with b_common
  lowerIndex = match(result,b_common)[1]
  
  #if lowerindex is a valid number there exists a capital
  if (!is.na(lowerIndex)) 
  {
    #find the uppercase word in 'common_capitals' using the same index
    sim_text_C[i]= common_capitals[lowerIndex]
  } 
  #if lowerindex is not a valid number there does not exist a capital word
  else 
  {
    #else leave it alone
    sim_text_C[i] = result
  }
}

#printing the text
cat(sim_text_C)
