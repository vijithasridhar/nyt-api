# Topic modeling with R? See what things articles talked about for Clinton in each year 1990-1994

# word cloud based on abstracts of NYT articles regarding Clinton by year

# idea/help credz to https://georeferenced.wordpress.com/2013/01/15/rwordcloud/ and 
# http://web.stanford.edu/~cengel/cgi-bin/anthrospace/scraping-new-york-times-articles-with-r

suppressMessages(library(RJSONIO))
suppressMessages(library(RCurl))
suppressMessages(library(ggplot2))
suppressMessages(library(tm))
suppressMessages(library(wordcloud))
suppressMessages(library(topicmodels))

dev.new(width=11, height=7)

api <- "fd71886491edcd0a22a2e93cd30c2be7:18:59638065"
pageRange <- 0:100          # max page is 100 - pagination past that isn't allowed rn
q <- "hillary+clinton"      # Query string, use + instead of space
moreStopwords <- c("hillary", "rodham", "clinton", "(s)", "new", "says", "can", "may", "u..", "senate", "senator")

N <- 5          # number of terms to compare wordcloud/LDA
startYear <- 2006
endYear <- 2010

for (y in startYear:endYear) {
    print(paste("Loading data for year", y))
    beginDate <- paste0(y, "0101")         #YYYYMMDD
    endDate <- paste0(y, "1231")           #YYYYMMDD

    dates <- c()
    headlines <- c()
    count <- 0
    for (i in pageRange) {
        uri <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=", q, 
                    "&page=", i, "&fl=pub_date,headline&begin_date=", beginDate,"&end_date=", endDate, "&api-key=", api)
        
        d <- getURL(uri)
        res <- fromJSON(d,simplify = FALSE)
        jend <- ifelse(length(res$response$docs) != 10, length(res$response$docs), 10)
        for (j in 1:jend) {
            # concat all dates/abstracts
            dates <- append(dates, unlist(res$response$docs[[j]]$pub_date))
            headlines <- append(headlines, unlist(res$response$docs[[j]]$headline$main))
            count <- count + 1
        }
        if (jend != 10) break
    }

    print(paste("YEAR:", y, "    Number of records:", count))       # for some reason \t isn't working; TODO look into this

    dateChars <- as.character(dates)
    yearCount <- substr(dateChars, 0, 4)        # dates by years
    monthCount  <- substr(dateChars, 0, 7)      # dates by months
    dateRangeStr <- paste0(as.Date(beginDate, "%m/%d/%Y"), " to ", as.Date(endDate, "%m/%d/%Y"))

    # plot counts of NYT articles per year
    #dev.new(width=11, height=7)
    #qplot(yearCount, xlab="Year", main=paste("NYT articles search term: ",q), sub=dateRangeStr, asp=0.3)

    absCorpus <- VCorpus(VectorSource(as.character(headlines)))
    absCorpus <- tm_map(absCorpus, stripWhitespace)
    absCorpus <- tm_map(absCorpus, content_transformer(tolower))
    absCorpus <- tm_map(absCorpus, removeWords, stopwords("english"))
    # absCorpus <- tm_map(absCorpus, stemDocument, lazy=TRUE)       # TODO: fix, some Rpoppler config issue is messing this up

    absCorpus <- tm_map(absCorpus, removeWords, moreStopwords)
    # print(absCorpus)

    layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
    par(mar=c(1,1,1,1))
    plot.new()
    text(x=0.5, y=0.5, paste0(" ", y))        # workaround for plot title
    wordcloud(absCorpus, scale=c(3,0.5), max.words=100, random.order=FALSE, rot.per=0.25, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

    # compare the wordcloud to what you get out of topic modeling
    
    # get top N words from wordcloud
    absDTM = DocumentTermMatrix(absCorpus)
    cloudMat <- as.matrix(absDTM)
    cloudSortedDF <- as.data.frame(sort(colSums(cloudMat), decreasing=TRUE))
    topWordsCloud <- as.character(head(row.names(cloudSortedDF), N))
    print("Top words from the wordcloud:")
    print(topWordsCloud)

    # start topic modeling
    # remove terms in less than 10 documents
    absDict <- as.character(findFreqTerms(absDTM, 10))
    absFiltered <- DocumentTermMatrix(absCorpus, list(dictionary=absDict))

    absMat <- as.matrix(absFiltered)
    topTerms <- sort(colSums(absMat), decreasing=TRUE)
    topTerms <- data.frame(terms=names(topTerms), headlines=topTerms)
    topTerms$terms <- reorder(topTerms$terms, topTerms$headlines)
    # print(topTerms)

    #remove rows of len 0
    rowCounts <- apply(absFiltered, 1, sum)
    absFiltered <- absFiltered[rowCounts > 0, ]     # for some reason the ", " at the end makes it work

    # plot top terms
    # ggplot(topTerms[1:10,], aes(x=terms, y=count)) + geom_point(size=5, colour="blue") + ggtitle(paste("Top terms in headlines in the year", y))

    absLDA <- LDA(absFiltered, 3)       # 3 topics
    print("Top topics from LDA: ")
    print(terms(absLDA, N))             # just out of curiosity, to see if there are multiple cohesive topics

    # get the first topic, and compare it 
    topicVector <- droplevels(as.data.frame(terms(absLDA, N))[, "Topic 1"])
    topWordsLDA <- as.character(topicVector)
    # print(topWordsLDA)
    diffWords <- paste(setdiff(topWordsCloud, topWordsLDA), collapse=", ")
    print(paste("Topic modeling and naive frequency result in", length(setdiff(topWordsLDA, topWordsCloud)), 
        "different word(s):", diffWords))
}
