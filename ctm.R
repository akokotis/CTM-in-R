#Correlated Topic Model

# Packages
require("data.table")
require("dplyr")
require("tidyr")
require("stringr")
require("tidytext")
require("stringi")


# Load data
data=read.csv(file.choose(), stringsAsFactors=F)
str(data)
columns=c("external_id","name","description","category_names")
filtered=data[,columns]


# Cleaning
to_remove=read.csv("C:/Users/AnneKokotis/Documents/General Code/Special Symbols.csv", colClasses=c("character"), stringsAsFactors=F)

for(i in 1:nrow(to_remove)){
		value=to_remove$Special_Symbol[i]
		replace=to_remove$Replace[i]
		filtered$description=apply(filtered[,c('description'), drop = FALSE],1,function(x) gsub(value,replace,x))
	}
	
	## &frasl; represents slash /     Used in measurements
    filtered$description=apply(filtered[,c('description'), drop = FALSE],1,function(x) gsub("&frasl;","/",x))

	filtered$description=apply(filtered[,c('description'), drop = FALSE],1,function(x) gsub("\\Q.\\E"," ",x))

	filtered$description=apply(filtered[,c('description'), drop = FALSE],1,function(x) gsub("\\Q-\\E"," ",x))


    filtered[,sapply(filtered,is.character)] <- sapply(
     filtered[,sapply(filtered,is.character)],
     iconv,"WINDOWS-1252","UTF-8")

# Reduce description to first 50 words; ALTERNATE-dock words after a certain symbol	 
subset= filtered %>%
		#mutate(Description=gsub("^((\\w+\\W+){49}\\w+).*$","\\1",description)) %>%
		mutate(Description=gsub("^(.*?)\\|.*","\\1",description)) %>%
		unite(combo,name,Description, sep=" ", remove=F) %>%
		select(external_id, name, category_names, Description, combo)

# Any products with blank titles?
nrow(subset[which(subset$name==""),])
subset[which(subset$name==""),"name"] = "unclassified"

# blank descriptions?
nrow(subset[which(subset$Description==""),])		
subset[which(subset$Description==""),"Description"] = "unclassified"

# blank combos?
nrow(subset[which(subset$combo==""),])		
subset[which(subset$combo==""),"combo"] = "unclassified"
		
		
# Stopword
# R Sourced
require("tm")
stopwords_vector=stopwords(kind = "en")
stopwords=data_frame(word=stopwords(kind = "en"))


#Topic Modeling Process
require("topicmodels")
require("servr")
require("LDAvis")
require("fpc")

original=subset

# Document Term Matrix funcion
createCorpus=function(product,stopwords_vector){
        docs=Corpus(VectorSource(product))
        docs=tm_map(docs,content_transformer(tolower))
        docs=tm_map(docs, removeWords, stopwords_vector)
        docs=tm_map(docs, stripWhitespace)
        docs[[1]]$content
        
        dtm <- DocumentTermMatrix(docs,control=list(stopwords=FALSE,
                           removePunctuation = FALSE,
                           removeNumbers = FALSE,
                           stemming = FALSE))
        return(dtm)
    }

# Document Term Matrices -- on name, on description, on name+description
dtm1=createCorpus(subset$name,stopwords_vector)
dtm2=createCorpus(subset$Description,stopwords_vector)
dtm3=createCorpus(subset$combo,stopwords_vector)


# Number of groups
k=30


# CTM
control_CTM_VEM <-
	list(nstart=15, seed = sample(100,15,replace=FALSE),
	var = list(iter.max = -1, tol = 10^-6),
	em = list(iter.max = -1, tol = 10^-4),
	initialize = "random",
	cg = list(iter.max = 1000, tol = 10^-5))

	# handle empty rows
	rowTotals1 <- apply(dtm1 , 1, sum)
	rowTotals2 <- apply(dtm2 , 1, sum)
	rowTotals3 <- apply(dtm3 , 1, sum)
	to_remove1=as.vector(unique(which(rowTotals1== 0)))
	to_remove2=as.vector(unique(which(rowTotals2== 0)))
	to_remove3=as.vector(unique(which(rowTotals3== 0)))
	
	dtm1_ctm=dtm1[rowTotals1> 0, ]
	dtm2_ctm=dtm2[rowTotals2> 0, ]
	dtm3_ctm=dtm3[rowTotals3> 0, ]
	
	if(length(to_remove1)>0){subset1=subset[-to_remove1,]}else{subset1=subset}
	if(length(to_remove2)>0){subset2=subset[-to_remove1,]}else{subset2=subset}
	if(length(to_remove3)>0){subset3=subset[-to_remove1,]}else{subset3=subset}
	
	# Run the CTM models
	system.time({
    run1=CTM(dtm1_ctm, k=k, control=control_CTM_VEM)
    theta1 <- posterior(run1)[["topics"]] %>% as.matrix
    ctm1=colnames(theta1)[apply(theta1,1,which.max)]
	})
	
	system.time({
    run2=CTM(dtm2_ctm, k=k, control=control_CTM_VEM)
    theta2 <- posterior(run2)[["topics"]] %>% as.matrix
    ctm2=colnames(theta2)[apply(theta2,1,which.max)]	
	})
	
	system.time({
    run3=CTM(dtm3_ctm, k=k, control=control_CTM_VEM)
    theta3 <- posterior(run3)[["topics"]] %>% as.matrix
    ctm3=colnames(theta3)[apply(theta3,1,which.max)]	
	})
	
	# Bind to original rows
    ctm_part1=cbind(data_frame(external_id=subset1$external_id),data_frame(ctm1=ctm1))
    ctm_part2=cbind(data_frame(external_id=subset2$external_id),data_frame(ctm2=ctm2))
	ctm_part3=cbind(data_frame(external_id=subset3$external_id),data_frame(ctm3=ctm3))
	

# K-Means
	system.time({
    krun1<- kmeans(dtm1, centers=k,nstart = 25,iter.max = 1000)
	})
	
	system.time({
	krun2<- kmeans(dtm2, centers=k,nstart = 15,iter.max = 500)
	})
	
	system.time({
	krun3<- kmeans(dtm3, centers=k,nstart = 15,iter.max = 500)
	})
	
	kmeans1=krun1$cluster
	kmeans2=krun2$cluster
	kmeans3=krun3$cluster
	
	kmean_part=cbind(data_frame(external_id=subset1$external_id),data_frame(kmeans1=kmeans1),data_frame(kmeans2=kmeans2),data_frame(kmeans3=kmeans3))

# Merge into final dataset	
combine1=merge(original,ctm_part1, by="external_id", all.x=TRUE)
combine2=merge(combine1,ctm_part2, by="external_id", all.x=TRUE)
combine3=merge(combine2,ctm_part3, by="external_id", all.x=TRUE)
combine4=merge(combine3,kmean_part, by="external_id", all.x=TRUE)

final_groups=combine4

# Label rows as unclassified if they are not a part of any cluster
final_groups[which(final_groups$ctm1==""),"ctm1"] = 0
final_groups[which(final_groups$ctm2==""),"ctm2"] = 0
final_groups[which(final_groups$ctm3==""),"ctm3"] = 0
final_groups[which(final_groups$kmeans1==""),"kmeans1"] = 0
final_groups[which(final_groups$kmeans2==""),"kmeans2"] = 0
final_groups[which(final_groups$kmeans3==""),"kmeans3"] = 0

# Save dataset as .csv
write.csv(final_groups, "clusters.csv")
