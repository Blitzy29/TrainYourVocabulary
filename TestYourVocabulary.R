require(data.table)
require(plyr)
require(dplyr)
require(magrittr)

dictionary <- fread('GermanEnglish.csv')
dictionary[,SucceedSession := FALSE]
dictionary[,TrySession := FALSE]

triesSession <- 0

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
  
  if (!isItCorrect) {
    print(paste0('Sorry, it was ',dictionary[rowToTest,German],' = ',dictionary[rowToTest,English]))
    correction <- readline("Write it again   ")
  }
  
  dictionary[rowToTest,':='(SucceedSession = isItCorrect,
                            Score = Score-(1*!isItCorrect) + (1*isItCorrect),
                            TrySession = TRUE)]
  triesSession %<>%  +1
  
}


dictionary[,Succeed := Score >= 5]

write.table(dictionary,
            file='GermanEnglish.csv',
            sep=";",
            row.names = FALSE)

nKnownWordsSession <- dictionary[,sum(SucceedSession)]
nTryWordsSession <- dictionary[,sum(TrySession)]
nKnownWords <- dictionary[,sum(SucceedSession)] + dictionary[,sum(Succeed)]
nWords <- nrow(dictionary)
nArchivedWords <- dictionary[,sum(Succeed)]

print(paste0("Your success rate is ",nKnownWordsSession,"/",triesSession,
      " so ",round(nKnownWordsSession/triesSession*100),"%."))
print(paste0("Over this session, you know ",nKnownWordsSession,"/",nTryWordsSession,
      " words, so ",round(nKnownWordsSession/nTryWordsSession*100),"%."))
print(paste0("You have ",nArchivedWords," archived words, so ",
             round(nArchivedWords/nWords*100),"% of the dictionary."))

# if (yourGuess == "STOP IT") print('Come back next time!')
if (yourGuess != "STOP IT") print('Congratulations! Your are not Jon Snow, you know everything.')



