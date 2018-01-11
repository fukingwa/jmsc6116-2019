---
title: "Lecture 1: Comparing Xi and Hu's Party Congress Speeches"
author: "King-wa Fu"
date: "January 10, 2018"
output: md_document
---
Chinese leader's speech in significant political event is a key for public and media to understand the country's strategy and future development. This short paper aims to analyze and compare two Chinese Communist Party leaders' speeches made at the Party Congress: Xi Jinping's speech at the 19th Party Congress (2017) and Hu jintao's the 17th party congress (2007).   

```{r setup, include=FALSE}
library("wordcloud")
library("tm")
knitr::opts_chunk$set(echo = TRUE)
```

First at all, let's obtain the copy of Xi Jinping's speech at the 19th Party Congress.

```{r xi, echo=TRUE}
con <- file("Xi.txt")
xi <- readLines(con)  # Read line by line from the connection to a string array xi
close(con) # Remember to close the connection after use
xi[1:3] # List the first three lines of Xi's speech
```

Next, we get the copy of Hu jintao's 17th Party Congress speech when he made 10 years ago.

```{r hu, echo=TRUE}
con <- file("Hu.txt")
hu <- readLines(con)
close(con)
#head(hu)
```

Then, we define a R function, namely Preprocessing, to convert all charactes to lower case, remove the texts (punctuations and "stopwords") we don't want, and "clean" the text data into the format for next step. The result shows the first three line of the "cleaned" version of Xi's speech.

```{r pp, include=TRUE, echo=TRUE}

Preprocessing <- function(sT){
  #create corpus
  r_stats_text_corpus <- Corpus(VectorSource(sT))
  #clean up
  r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ","abcdefghijklmnopqrstuvwxyz",x))### Convert to lower case
  r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation)   ### remove punctuation
  r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords("english")))  #### remove stopwords
  return(r_stats_text_corpus)
}

xi.p <- Preprocessing(xi) # Send the array xi to the function for data cleaning
hu.p <- Preprocessing(hu) # Output the results to xi.p and hu.p (class "Corpus")
inspect(xi.p[3]) # Inspect the first three lines of Xi's speech
```


Ok. So now we compare the two speeches by using histogram. As a first step, a term frequency array showing the word used (by rows) and the two leaders (by columns) is created.  

```{r hg, echo=TRUE}
xi.combined <- paste(sapply(1:length(xi.p), function(x) xi.p[[x]]$content), collapse=" ") # Combine all lines into a meta document
hu.combined <- paste(sapply(1:length(hu.p), function(x) hu.p[[x]]$content), collapse=" ")

tdm <- TermDocumentMatrix(Corpus(VectorSource(c(xi.combined,hu.combined)))) # Create a term document matrix
tdm <- as.matrix(tdm) # in a standard matrix
colnames(tdm) <- c("Xi's speech","Hu's speech")
```

Here you go. The Top-5 high frequency term plot is created.

``` {r bp, echo=TRUE}

Num_of_terms_shown <- 5
par(mfrow=c(1,2)) # 1x2 panel plot
barplot(tdm[order(tdm[,"Xi's speech"], decreasing=TRUE)[1:Num_of_terms_shown],"Xi's speech"],cex.names=0.4,col="red")
title("Xi Jinping's 19th Congress speech")
barplot(tdm[order(tdm[,"Hu's speech"], decreasing=TRUE)[1:Num_of_terms_shown],"Hu's speech"],cex.names=0.4,col="blue")
title("Hu jintao's 17th Congress speech")
```

Question: is it a fair comparison? How can we make a better plot?
Hints:
```{r q, echo=TRUE}
print(paste0("Total number of terms (Xi):",sum(tdm[,"Xi's speech"])))
print(paste0("Total number of terms (Hu):",sum(tdm[,"Hu's speech"])))
```

Next, we visualize the text frequency by using wordcloud function.

```{r wc, echo=TRUE}

par(mfrow=c(1,2)) # 1x2 panel plot
wordcloud(xi.p, scale=c(3,.2),min.freq=50, max.words=Inf, random.order=F, colors=brewer.pal(8, "Accent"))   
title("Xi Jinping's 19th Congress speech")
wordcloud(hu.p, scale=c(3,.2),min.freq=50, max.words=Inf, random.order=F, colors=brewer.pal(8, "Dark2"))   
title("Hu jintao's 17th Congress speech")
```

Finally, we create a comparison wordcloud, which compares the relative frequency with which a term was used in the two documents. For example, Hu used the word "scientific" more frequenctly than Xi and then the word cloud prints "scientific" in the side of Hu. By the same token, you find "dream" is closer to Xi. The plot shows the difference between the word usage in the leader's speeches and sheds light on the style of the leaders. 

```{r cw, echo=TRUE, message=FALSE, warning=FALSE}

comparison.cloud(tdm,max.words=100,random.order=FALSE, colors=c("red","blue"))
```




