# Build a hyperlink to government jobs based on user's search

getLink <- function(x) {
    
    linkBase <- "https://www.tidytextmining.com/topicmodeling.html#document-topic-probabilities" 
    
    space <- "%20"
    
    term <- as.character(x)
    
    newTerm <- gsub(" ", space, term)
    
    link <- paste0(linkBase, newTerm)
    
    return(link)
}