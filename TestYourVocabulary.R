require(data.table)
# require(plyr)
# require(dplyr)
# require(magrittr)

dictionary <- fread('GermanEnglish.csv')
dictionary[,SucceedSession := FALSE]

while ((dictionary[,sum(SucceedSession)] + dictionary[,sum(Succeed)]) != nrow(dictionary)) {
  
  # Pick a random word
  rowToTest <- sample(nrow(dictionary),1)
  if (dictionary[rowToTest,SucceedSession] | dictionary[rowToTest,Succeed]) next()
  
  germanOrEnglish <- sample(c("German","English"),1)
  
  if (germanOrEnglish == "English") {
    yourGuess <- readline(paste0("What is the English for '",dictionary[rowToTest,German],"'?   "))
    isItCorrect <- yourGuess == dictionary[rowToTest,English]
  }
  
  if (germanOrEnglish == "German") {
    yourGuess <- readline(paste0("What is the German for '",dictionary[rowToTest,English],"'?   "))
    isItCorrect <- yourGuess == dictionary[rowToTest,German]
  }
  
  if (yourGuess == "STOP IT") break()
  
  if (!isItCorrect) print(paste0('Sorry, it was ',
                          dictionary[rowToTest,German],
                          ' = ',
                          dictionary[rowToTest,English]))
  dictionary[rowToTest,':='(SucceedSession = isItCorrect,
                            Score = Score-(1*!isItCorrect) + (1*isItCorrect))]
  
}

if (yourGuess == "STOP IT") print('Come back next time!')
if (yourGuess != "STOP IT") print('Congratulations! Your are not Jon Snow, you know everything.')

dictionary[,Succeed := Score >= 10]

write.table(dictionary,
            file='GermanEnglish.csv',
            sep=";",
            row.names = FALSE)

