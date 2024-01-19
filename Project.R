library(gutenbergr)
library(tidyverse)
library(tidytext)
library(gtools)
library(xfun)
library(tm)
library(textstem)
library(textdata)
library(wordcloud)
library(widyr)
library(igraph)
library(ggraph)
library(cowplot)
library(reshape2)
library(purrr)
library(topicmodels)
library(ldatuning)
library(FactoMineR)
library(stats)
library(factoextra)
library(gt)
library(ggsci)
library(NbClust)
library(cluster)
library(fpc)
library(clue)
library(xtable)
library(tikzDevice)

setwd("D:/Uni/3.1 - LAB 1/")
load("complete_wp.RData")

###########################################################################
###########################################################################
###                                                                     ###
###                          TEXT MANIPULATION                          ###
###                                                                     ###
###########################################################################
###########################################################################

# Starting by Downloading the Book
# 
# gutenberg_metadata %>% 
#   filter(title == "War and Peace") %>% 
#   select(gutenberg_id)

# warpeace <- gutenberg_download(gutenberg_id = 2600) 


warpeace <- warpeace %>% 
  mutate(books = ifelse(str_detect(text, "(?<=BOOK )")
                        ,str_extract(text, "(?<=BOOK )[A-Z]*"),
                        NA),
         epilogue = ifelse(str_detect(text, "[A-Z]*\\s(EPILOGUE)"),
                           ifelse(str_extract(text, "[A-Z]*(?= EPILOGUE)")=="FIRST","SIXTEEN","SEVENTEEN"),
                           NA)
  ) %>% 
  mutate(
    book=if_else(
      !is.na(epilogue),
      epilogue,
      books
    ),
    # chapter = ifelse(str_detect(text, "(?<=CHAPTER )"),
    #                  roman2int(str_extract(text, "(?<=CHAPTER )[A-Z]*")),
    #                  NA)
    chapter = cumsum(str_detect(text,regex("(?<=chapter)", ignore_case = T)))-365
  )%>% 
  select(-books, -epilogue) %>% 
  fill(c(book, chapter), .direction = "down") 

# Factoring Book levels

levels <- str_to_upper(numbers_to_words(c(1:17)))

warpeace$book <-factor(warpeace$book,
                       levels= levels, 
                       labels = c(1:17)) 

warpeace$book <- as.integer(warpeace$book)

warpeace <- warpeace %>% 
  mutate(book = as.numeric(book)) %>% 
  mutate(volume = case_when(
    book<4 ~ 1,
    book>3 & book<9 ~ 2,
    book>8 & book<12 ~ 3,
    book>11 & book<16 ~ 4,
    TRUE ~ 5
  )) %>% 
  relocate(volume, .before = "book") %>% 
  select(-gutenberg_id)

# Filtering for only sentences


warpeace <- warpeace %>% 
  filter(!is.na(book)) %>% 
  filter(str_detect(text,"\\S")) %>% 
  filter(!str_detect(text, "[A-Z]{4,}\\s([IVXLCM]+\\s)?")) %>% 
  filter(!str_detect(text, '[0-9]')) %>% 
  group_by(volume,book, chapter) %>% 
  summarise(text = paste(text, collapse = " ")) %>% 
  ungroup() %>% 
  as.tibble()



wptoken <- warpeace %>% 
  group_by(volume,book,chapter) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  ungroup()

wptoken <- wptoken %>% 
  mutate(
    word=ifelse(
      str_detect(word, "’(?=s)")==T, 
      str_replace_all(word,"’s", ""),
      word
    )
  ) %>% 
  mutate(
    volume = factor(volume,
                    levels = c(1,2,3,4,5),
                    labels = c(
                      "volume 1",
                      "volume 2",
                      "volume 3",
                      "volume 4",
                      "epilogues"
                    )))



wp_bigram <- warpeace %>%
  group_by(volume,book,chapter) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

wp_filtered <- wp_bigram %>% 
  separate(bigram, c("w1","w2"), sep = " ") %>% 
  filter(!w1 %in% stop_words$word) %>% 
  filter(!w2 %in% stop_words$word) %>%
  mutate(
    w2=ifelse(
      str_detect(w2, "’(?=s)")==T, 
      str_replace_all(w2,"’s", ""),
      w2
    )
  ) %>% 
  unite(bigram, w1,w2,sep = " ") %>% 
  ungroup()

# Creating .txt files for war and peace full book
# setwd("D:/Uni/3.1 - LAB 1/WarAndPeace/")
# for (i in 1:nrow(warpeace)) {
#   filename <-  str_c(c("book_", warpeace$book[i],"_chapter_",warpeace$chapter[i],".txt"),sep = "_", collapse = "")
#   writeLines(warpeace$text[i], con = filename)
# }
# setwd("D:/Uni/3.1 - LAB 1/")

wpCorpus <- Corpus(DirSource("WarAndPeace"))

wp_stopwords <- wptoken %>% 
  distinct(book, word) %>% 
  count(word, sort = T) %>% 
  filter(n ==17) 

dtm <- wptoken %>% 
  # anti_join(wp_stopwords) %>%
  count(chapter, word) %>% 
  as_tibble() 

dtm <- dtm %>% 
  cast_dtm(chapter, word, n)



###########################################################################
###########################################################################
###                                                                     ###
###                        OVERALL BOOK ANALYSIS                        ###
###                                                                     ###
###########################################################################
###########################################################################

# Descriptive Analysis ####
dtm %>% 
  tidy() %>% 
  mutate(document = as.numeric(document)) %>% 
  group_by(term) %>% 
  count() %>% 
  ggplot(aes(x=n))+
  geom_histogram( fill = "grey", col = "black")+
  theme_minimal()+
  xlab("n")

 wptoken %>% 
  group_by(volume, chapter) %>% 
  count(word) %>% 
  summarize(n_book_word = sum(n)) %>% 
  group_by(volume) %>% 
  summarize(Chapters = n(),`Total Word Count` = sum(n_book_word))

print(xtable(x = table1,type = "latex"))

wptoken %>% 
  group_by(volume,book,chapter) %>% 
  count(word,book) %>%
  group_by(chapter) %>% 
  mutate(n.book = sum(n)) %>% 
  ggplot(aes(volume, n.book, group = book, fill = factor(volume),
             alpha = 0.4)) +
  geom_boxplot(show.legend = F)+
  facet_wrap(~volume, scales = "free_x", nrow = 1)+
  theme(axis.text.x=element_blank())+
  geom_hline(yintercept = 631,
             col = "red",
             linetype = "dashed", linewidth = 0.1)+
  theme_minimal()

total <- wptoken %>% 
  count(volume,word) %>% 
  group_by(volume) %>% 
  summarize(total= sum(n))

wptoken %>%
  group_by(volume) %>% 
  count(word) %>% 
  left_join(total) %>% 
  ggplot(aes(n/total, fill = volume))+
  geom_histogram(show.legend = F)+
  facet_wrap(~volume, nrow = 1)+
  theme_minimal()+
  xlab("frequency")
  xlim(NA, 0.005)

# Analyzing top words. Looking for main characters ####

wptoken %>%
  count(word, sort = T) %>% 
  top_n(10) %>% 
  ggplot(aes(reorder(word,n), n))+
  geom_col(fill = c(
    "#7876B1FF",
    "#0072B5FF",
    "#7876B1FF",
    "#7876B1FF",
    "#20854EFF",
    "#0072B5FF",
    "#20854EFF",
    "#20854EFF",
    "#20854EFF",
    "#0072B5FF"
    ))+
  theme_minimal()+
  coord_flip()+
    xlab(NULL)+
    ylab(NULL)+
    scale_x_discrete(position = "top")+
    scale_y_reverse()+
    theme(
      axis.text.y = element_text(hjust = 1,size = (25))
    )
  

# Eyes by chapter

obj <- wptoken %>% 
  filter(word %in% "eyes") %>%
  group_by(volume, book) %>% 
  count(word) %>% 
  group_by(volume) %>% 
  summarize(
    tot = sum(n),
    mean = mean(n),
    sd = sd(n)
  )
sum(obj$tot)



wptoken %>% 
  filter(word %in% "eyes") %>%
  group_by(volume,chapter) %>% 
  filter(volume != 5) %>% 
  count(word, chapter) %>% 
  ggplot(aes(chapter, n, col = volume))+
  geom_point(show.legend = F,size = 2)+
  geom_smooth(show.legend = F, size = 2, se = F)+
  geom_vline(xintercept = c(68,167,265),
             colour = "black",
             linetype = "dashed")+
  ylim(0,11)+
  theme_minimal()+
  ylab("")

wptoken %>% 
  filter(word %in% "eyes") %>%
  group_by(volume,chapter) %>% 
  filter(volume != "epilogues") %>% 
  count(word, chapter) %>% 
  ggplot(aes(chapter, n, col = volume))+
  geom_point(show.legend = F,size = 2)+
  geom_smooth(show.legend = F, size = 2, se = F)+
  geom_vline(xintercept = c(68,167,265),
             colour = "black",
             linetype = "dashed")+
  ylim(0,11)+
  theme_minimal()+
  ylab("")

table2 <- wp_filtered %>% 
  separate(bigram, c("w1","w2"), sep= " ") %>% 
  filter(w1 == "eyes" | w2 == "eyes") %>% 
  count(w1,w2, sort = T) %>% 
  top_n(8) %>% 
  unite(bigram, c(w1, w2), sep = " ") 

print(xtable(x = table2,type = "latex"))

  

# Back to top words

wptoken %>%
  group_by(volume) %>% 
  count(word, sort = T) %>% 
  top_n(5) %>% 
  mutate(vol = ifelse(
    str_sub(volume, 8) == "es", 
    as.character(5),
    str_sub(volume,  8)
  )) %>% 
  ggplot(aes(reorder_within(word,n, vol, sep = " v."), n,fill = volume))+
  geom_col(show.legend = F)+
  coord_flip()+
  facet_wrap(~volume, scales = "free", nrow = 1)+
  theme_minimal()+
  xlab(NULL)+
  ylab(NULL)+
  theme(
    axis.text.y = element_text(hjust=1, size = (15))
  )



# Let's also analyze the top bigrams
wp_filtered %>%
  count(bigram) %>% 
  top_n(10) %>% 
  ggplot(aes(reorder(bigram,n),n)) +
  geom_col(fill = "#20854EFF")+
  coord_flip()
 

# bind_tf_idf() ####

wptoken %>% 
  count(volume, word, sort = T) %>% 
  bind_tf_idf(word, volume, n) %>% 
  group_by(volume) %>% 
  top_n(5, tf_idf) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, tf_idf)) %>% 
  ggplot(aes(reorder(word,n),n, fill = factor(volume)))+
  geom_col(show.legend = F)+
  facet_wrap(~volume, scales = "free", nrow = 1)+
  coord_flip()+
  theme_minimal()+
  ylab(NULL)+
  xlab(NULL)+
  theme(
    axis.text.y = element_text(hjust=1, size = (15))
  )


# Counting Most Unique Words by Volume
wptoken %>% 
  count(volume, word, sort = T) %>% 
  bind_tf_idf(word, volume, n) %>% 
  top_n(500,tf_idf) %>% 
  mutate(word = reorder(word, n)) %>%
  group_by(volume) %>% 
  summarize(n=n()) 


# Most common sentiment for the entire book ####
wptoken %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  gather(sentiment, value,anger:trust) %>% 
  arrange(-value) %>% 
  top_n(7) %>% 
  ggplot(aes(reorder(sentiment, value),value, fill = c(
    "#20854EFF",
    "#BC3C29FF",
    "#20854EFF",
    "#20854EFF",
    "#BC3C29FF",
    "#20854EFF",
    "#BC3C29FF"
  ))) + 
  geom_col(show.legend = F) +
  coord_flip()+
  theme_minimal()+
  ylab(NULL)+
  xlab(NULL)+
  theme(
    axis.text.y = element_text(hjust=1, size = (25))
  )


# Wordcloud by first 4 volumes ####
wp_sent_count <- wp_sent %>%
  group_by(volume) %>% 
  count(word, sentiment) 

par(mar = c(0, 0, 0, 0), mfrow = c(2, 2))
for (i in 1:4){
  wps1 <- filter(wp_sent_count, volume == i)
  wps2 <- acast(wp_sent_count, word ~ sentiment, value.var = "n", fill = 0)
  comparison.cloud(wps2, colors = c("darkred", "darkgreen"),max.words = 500,
                   title.size = 0.7,scale=c(2,1), rot.per = 0)+
    text(x = 0.5, y = 1, cex = 2, paste('volume', i))
}



#  Sentiment Composition of chapter: RPlot_sent

wptoken %>% 
  inner_join(get_sentiments("nrc")) %>% 
  group_by(chapter,sentiment) %>% 
  count(sentiment) %>% 
  group_by(chapter) %>% 
  top_n(1,n) %>% 
  ggplot(aes(chapter, fill =sentiment)) +
  geom_bar(position = "dodge",
           show.legend =F)+
  geom_vline(xintercept = c(68,167,265,339),
             colour = "indianred",
             linetype = "twodash",
             size = 2)+
  scale_fill_manual(values = c(
    fear = "#F8766D",
    negative = "#7CAE00",
    positive = "#00BFC4",
    trust = "#F564E3"
  ))+
  theme_minimal()+
  ylab(NULL)+
  xlab(NULL)

wptoken %>% 
  inner_join(get_sentiments("nrc")) %>% 
  group_by(chapter,sentiment) %>% 
  filter(sentiment != "positive") %>%
  filter(sentiment != "negative") %>%
  count(sentiment) %>% 
  group_by(chapter) %>% 
  top_n(1,n) %>% 
  ggplot(aes(chapter, fill =sentiment)) +
  geom_bar(position = "dodge",
           show.legend = T)+
  geom_vline(xintercept = c(68,167,265,339),
             colour = "black",
             linetype = "twodash",
             size = 1.2)+
  scale_fill_manual(values = c(
    fear = "#F8766D",
    anticipation = "#9E9E9E",
    sadness = "#619CFF", 
    trust = "#F564E3",
    joy = "#F5C710"))+
  theme_minimal()+
  ylab(NULL)+
  xlab(NULL)


wptoken %>% 
  inner_join(get_sentiments("nrc")) %>% 
  group_by(book,chapter,sentiment) %>% 
  count(sentiment) %>% 
  group_by(chapter) %>% 
  top_n(1,n) %>% 
  group_by(book, sentiment) %>% 
  count(sentiment) %>% 
  group_by(book) %>% 
  mutate(sent.perc = n/sum(n)) %>% 
  arrange(book, -sent.perc) %>% 
  ggplot(aes(book,sent.perc, fill = sentiment)) +
  geom_col(position = "stack",
           show.legend = T
           )+
  theme_minimal()+
  scale_fill_manual(values = c(
    fear = "#F8766D",
    negative = "#7CAE00",
    positive = "#00BFC4",
    trust = "#F564E3"
  ), name = NULL)+
  ylab(NULL)+
  xlab(NULL)+
  theme(
    legend.key.height = unit(2, 'cm'), 
    legend.key.width = unit(1, 'cm'),
    legend.text = element_text(size=25)
  )

wptoken %>% 
  inner_join(get_sentiments("nrc")) %>% 
  filter(sentiment != "positive") %>%
  group_by(book,chapter,sentiment) %>% 
  count(sentiment) %>% 
  group_by(chapter) %>% 
  top_n(1,n) %>% 
  group_by(book, sentiment) %>% 
  count(sentiment) %>% 
  group_by(book) %>% 
  mutate(sent.perc = n/sum(n)) %>% 
  arrange(book, -sent.perc) %>% 
  ggplot(aes(book,sent.perc, fill  = sentiment)) +
  geom_col(position = "stack",
           show.legend = T
           )+
  scale_fill_manual(values = c(
    fear = "#F8766D",
    negative = "#7CAE00",
    anticipation = "#9E9E9E",
    sadness = "#619CFF", 
    trust = "#F564E3",
    joy = "#F5C710"),
    labels = c("anticip.", "fear", "joy","negative","sadness","trust"),
    name = NULL)+
  theme_minimal()+
  ylab(NULL)+
  xlab(NULL)+
  theme(
    legend.key.height = unit(2, 'cm'), 
    legend.key.width = unit(1, 'cm'),
    legend.text = element_text(size=25)
  )


### Lexicon composition ####

table3 <- get_sentiments("nrc") %>% 
  count(sentiment, sort = T) %>% 
  mutate(perc = paste(round(n/sum(n)*100,0),"%"),
         perc1 = n/sum(n)*100,
         cumulative = paste(round(cumsum(perc1),1),"%")) %>% 
  select(-perc1) 

print(xtable(x = table3,type = "latex"))


### Separate

wptoken %>% 
  inner_join(get_sentiments("nrc")) %>% 
  filter(volume != "epilogues") %>%
  count(sentiment, chapter) %>% 
  group_by(sentiment) %>% 
  filter(sentiment %in% c("positive","fear", "trust", "negative")) %>%
  ungroup() %>% 
  group_by(chapter) %>% 
  arrange(chapter) %>% 
  mutate(n2=n/sum(n)) %>% # divido per le occorrenze totali che avvengono nel capitolo
  ggplot(aes(chapter,n2, group = sentiment))+
  geom_smooth(aes(col = sentiment), se = F, show.legend = F, size = 2, span = 0.5)+
  scale_color_manual(values = c(
    fear = "#F8766D",
    negative = "#7CAE00",
    positive = "#00BFC4",
    trust = "#F564E3"), name = NULL)+
  ylab(NULL)+
  xlab(NULL)+
  theme_minimal()+
  geom_vline(xintercept = c(68,167,265,339),
             colour = "black",
             linetype = "dashed")

# Pesato per le occorrenze totali del sentimento
wptoken %>% 
  inner_join(get_sentiments("nrc")) %>% 
  filter(volume != "epilogues") %>%
  count(sentiment, chapter) %>% 
  group_by(sentiment) %>% 
  filter(sentiment %in% c("positive","fear", "trust", "negative")) %>%
  ungroup() %>% 
  group_by(sentiment) %>% 
  mutate(aa=n/sum(n)) %>% # divido per il numero totale di occorrenze, tengo conto dell'impatto 
  ungroup() %>% 
  group_by(chapter) %>% 
  arrange(chapter) %>% 
  mutate(n2=aa/sum(aa)) %>% # divido per le occorrenze totali che avvengono nel capitolo
  ggplot(aes(chapter,n2, group = sentiment))+
  geom_smooth(aes(col = sentiment), se = F, size = 2, span = 0.55)+
  scale_color_manual(values = c(
    fear = "#F8766D",
    negative = "#7CAE00",
    positive = "#00BFC4",
    trust = "#F564E3"), name = NULL)+
  theme_minimal()+
  ylab(NULL)+
  xlab(NULL)+
  theme(
    legend.key.width = unit(0.5, 'cm'),
    legend.text = element_text(size=15)
  )+
geom_vline(xintercept = c(68,167,265,339),
             colour = "black",
             linetype = "dashed")

# Una parola positiva è più comune e avrà un impatto minore, considerando che sono 
# più frequenti


# Networks ####

# Bigram Count
wp_filtered %>% 
  separate(bigram, c("w1","w2"), sep = " ") %>% 
  count(w1,w2, sort = T) %>% 
  filter(n>50) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = 'fr')+
  geom_node_point(col = "lightblue", size = 4)+
  geom_edge_link(aes(edge_alpha = n),
                 edge_colour = "darkred",
                 arrow = arrow(angle = 20,
                               length = unit(2.5, 'mm'),
                               type = "closed"),
                 show.legend = F)+
  geom_node_text(aes(label = name), 
                 repel = T,
                 point.padding= unit(0.2, "lines"),
                 size = 6)+
  theme_void()

# Correlation 
wptoken %>% 
  group_by(word) %>% 
  filter(n()>=50) %>% 
  pairwise_cor(word,chapter, sort = T) %>%
  filter(correlation > .48) %>% 
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation,
                     edge_width = correlation),
                 edge_colour = "darkred",,
                 show.legend = F)+
  geom_node_point(col = "lightblue", size = 4)+
  geom_node_text(aes(label = name), 
                 repel = T,
                 point.padding= unit(0.2, "lines"),
                 size = 6)+
  theme_void()

families <- c("kurágin","bolkónski",	
              "rostóv","bezúkhov")

wptoken %>% 
  group_by(word) %>% 
  filter(n()>=50) %>% 
  pairwise_cor(word,chapter, sort = T) %>%
  # filter(item1 %in% characters | item1 %in% titles) %>% 
  filter(item1 %in% families) %>%
  filter(correlation > .27) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation,
                     edge_width = correlation),
                 edge_colour = "darkred",,
                 show.legend = F)+
  geom_node_point(col = "lightgreen", size = 4)+
  geom_node_text(aes(label = name), 
                 repel = T,
                 point.padding= unit(0.2, "lines"))+
  theme_void()



############################################################################
############################################################################
###                                                                      ###
###                     ANALYSIS FOR MAIN CHARACTERS                     ###
###                                                                      ###
############################################################################
############################################################################

main <- c ("pierre", "natásha", "andrew", "rostóv", "nicholas", "mary")    

wptoken %>% 
  filter(word %in% main) %>% 
  count(word,sort=T)


wptoken %>%
  filter(word %in% main) %>% 
  count(word,chapter) %>% 
  ggplot(aes(chapter,n))+
  geom_point()+
  geom_smooth(span = 0.7, aes(
    col = word,
    # fill = word
    ), show.legend = F)+
  facet_wrap(~word, scales = "free_x",
             labeller = labeller(word = 
                                   c("andrew" = "Prince Andrej Bolkonskij (n= 1138)",
                                     "pierre" =  "Pierre Bezuchov (n= 1959)",
                                     "nicholas" = "Nikolaj Rostov (n= 625)",
                                     "rostóv" = "Count Ilja Rostov (n= 772)",
                                     "mary" = "Marja Bollkonskaja (n= 665)",
                                     "natásha" = "Natalja Rostov (n= 1209)")))+
  geom_vline(xintercept = c(68,167,265),
             colour = "red",
             linetype = "dashed")+
  ylim(c(0,40))+
  theme(
    panel.background = element_rect(fill = NA,  colour = "grey20"),
    panel.grid = element_line(colour = "grey90")
        )+
  ylab(NULL)+
  xlab(NULL)

  wptoken %>%
  group_by(volume, chapter) %>% 
  filter(word %in% top3) %>% 
  count(word,chapter) %>% 
  group_by(word, volume) %>% 
  summarize(count=sum(n)) %>% 
  summarize(perc = count/sum(count))
  



# We are going to analyze the top 3 charachters

top3 <- c("pierre", "natásha", "andrew" )

wp_filtered %>% 
  separate(bigram, c("w1","w2"), sep = " ") %>% 
  count(w1,w2, sort = T) %>% 
  filter(w1 %in% top3) %>% 
  group_by(w1) %>% 
  top_n(5) %>% 
  mutate(w3=case_when(
    w1 == "pierre" ~ "P.",
    w1 == "natásha" ~ "N.",
    TRUE ~ "A.",
  )) %>% 
  mutate(w2 = reorder_within(w2, n,w3, sep = " ")) %>%
  arrange(-n) %>% 
  ggplot(aes(w2,n, fill = w1)) +
  geom_col(show.legend = F)+
  coord_flip()+
  facet_wrap(~w1, scales = "free",
            labeller = labeller(w1 = c("andrew" = "Andrej Bolkonskij",
                                       "pierre" =  "Pierre Bezuchov",
                                       "natásha" = "Natalja Rostov"))
  )+
  xlab(NULL)+
  theme_minimal()+
  ylab(NULL)

  
# Let's extract the top 3 bigram, luckily regargarding charchters
top3_bigram <- c("prince andrew","prince vasíli","princess mary")

wp_filtered %>%
  filter(bigram %in% top3_bigram) %>% 
  count(bigram,chapter) %>% 
  ggplot(aes(chapter,n))+
  geom_point()+
  geom_smooth(span = 0.8)+
  facet_wrap(~bigram, scales = "free_x")+
  geom_vline(xintercept = c(68,167,265),
             colour = "red",
             linetype = "dashed")

# Vasili isn't really that important for the plot...

# Correlated words with these characters ####

# Main + top corr + whole bokk
wptoken %>% 
  group_by(word) %>% 
  filter(n()>=50) %>% 
  pairwise_cor(word,chapter, sort = T) %>% 
  filter(item1 %in% top3) %>% 
  group_by(item1) %>%
  top_n(20) %>% 
  ungroup() %>% 
  ggplot(aes(reorder_within(item2,correlation, item1), correlation, fill = correlation)) + 
  geom_col(show.legend = F) +
  scale_x_reordered()+
  facet_wrap(~item1, scales = "free_y") +
  coord_flip()+
  geom_hline(yintercept = 0.2,
             col = "red",
             linetype = "dashed")+
  
  ylab(NULL)+
  xlab(NULL)+
  theme_minimal()+
  theme(strip.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 15))

# Main +  least corr + whole book
wptoken %>% 
  group_by(word) %>% 
  filter(n()>=50) %>% 
  filter(volume != "epilogues") %>% 
  pairwise_cor(word,chapter, sort = T) %>% 
  filter(item1 %in% top3) %>% 
  group_by(item1) %>%
  top_n(-7) %>% 
  ungroup() %>% 
  ggplot(aes(reorder(item2,correlation), correlation, fill = -correlation)) + 
  geom_col(show.legend = F) +
  scale_x_reordered()+
  facet_wrap(~item1, scales = "free_y") +
  coord_flip()+
  geom_hline(yintercept = -0.2,
             col = "red",
             linetype = "dashed")+
  ylab(NULL)+
  xlab(NULL)+
  theme_minimal()+
  theme(
    strip.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 15))


# Correlated word by volume with main ####
cor_plot_4 <- wptoken %>% 
  group_by(word) %>% 
  filter(n()>=50) %>% 
  filter(volume != "epilogues") %>% 
  pairwise_cor(word,chapter, sort = T) %>% 
  filter(item1 %in% top3) %>% 
  group_by(item1) %>%
  top_n(7) %>% 
  ungroup() %>% 
  ggplot(aes(reorder(item2,correlation), correlation, fill = correlation)) + 
  geom_col(show.legend = F) +
  scale_x_reordered()+
  facet_wrap(~item1, scales = "free_y") +
  coord_flip()+
  geom_hline(yintercept = 0.2,
             col = "red",
             linetype = "dashed")+
  ylab(NULL)+
  xlab(NULL)

plot_grid(cor_plot_1,
          cor_plot_2,
          cor_plot_3,
          cor_plot_4)



# Sentiment Associated with main charachters in the book geom_col() ####

wp_pair_sent <- wptoken %>%
  pairwise_count(word,chapter) %>% 
  inner_join(get_sentiments("nrc"), by= c(item2="word"))

sent_count <- wptoken %>% inner_join(get_sentiments("nrc")) %>% count(sentiment)

wp_pair_sent %>% 
  filter(item1 %in% top3) %>% 
  spread(sentiment, n, fill = 0) %>% 
  group_by(item1) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  gather(sentiment, value,anger:trust) %>% 
  group_by(item1) %>% 
  arrange(-value) %>% 
  left_join(sent_count) %>% 
  mutate(perc = value/n) %>% 
  top_n(5, perc) %>% 
  ggplot(aes(reorder_within(sentiment, perc, item1),perc, fill = item1)) + 
  geom_col(show.legend = F) +
  facet_wrap(~item1, scales = "free_y",
             labeller = labeller(item1 = c("andrew" = "Andrej Bolkonskij",
                                        "pierre" =  "Pierre Bezuchov",
                                        "natásha" = "Natalja Rostov")
                                 )
             )+
  coord_flip()+
  ylab(NULL)+
  xlab(NULL)+
  theme_minimal()
  

# It doesnt differ from the book itself

wptoken %>% 
  count(word,chapter) %>% 
  filter(word %in% c("music", "violin")) %>% 
  ggplot(aes(chapter,n))+
  geom_point()+
  geom_smooth(span = 0.6)+
  geom_vline(xintercept = c(68,167,265,339),
             colour = "black",
             linetype = "dashed")

# Count of neg/pos word by chapter for whole book ####
wptoken %>% 
  inner_join(get_sentiments("nrc")) %>% 
  filter(volume != "epilogues") %>% 
  count(sentiment, chapter) %>% 
  group_by(sentiment) %>% 
  filter(sentiment %in% c("positive","negative")) %>%
  ggplot(aes(chapter, n, group = sentiment))+
  geom_smooth(aes(col = sentiment), se = F)
 


# Count associated word by chapter and sentiment for main characters####
wp_pair_count_chapter <- wptoken %>% 
  mutate(CH=chapter) %>%
  filter(volume!=5) %>% 
  group_by(CH) %>% 
  pairwise_count(word,chapter) %>% 
  inner_join(get_sentiments("nrc"), by= c(item2="word")) 

# Count of fear and joy by chapter
wp_pair_count_chapter %>% 
  filter(item1 %in% top3) %>% 
  filter(sentiment %in% c("positive","joy","fear")) %>% 
  spread(sentiment, n, fill = 0) %>% 
  group_by(CH,item1) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  gather(sentiment, value,positive:fear) %>% 
  ggplot(aes(CH, value, group = sentiment))+
  geom_smooth(aes(col = sentiment), se = F)+
  facet_wrap(~item1, scales = "free_y", ncol = 2,
             labeller = labeller(item1 = c("andrew" = "Andrej Bolkonskij",
                                           "pierre" =  "Pierre Bezuchov",
                                           "natásha" = "Natalja Rostov")
             )
  )+
  xlab("chapter")

# Count weigthed for composition of the chapter by sentiment
wp_pair_count_chapter %>% 
  filter(item1 %in% top3) %>% 
  filter(sentiment %in% c("sadness","joy","fear")) %>%
  spread(sentiment, n, fill = 0) %>% 
  group_by(CH,item1) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  gather(sentiment, value,fear:sadness) %>% 
  arrange(CH) %>% 
  group_by(CH) %>% 
  mutate(perc = value/sum(value)) %>% # divido per le occorrenze totali che avvengono nel capitolo
  ggplot(aes(CH, perc, group = sentiment))+
  geom_smooth(aes(col = sentiment), se = F)+
  facet_wrap(~item1, scales = "free_y", ncol = 3,
             labeller = labeller(item1 = c("andrew" = "Andrej Bolkonskij",
                                           "pierre" =  "Pierre Bezuchov",
                                           "natásha" = "Natalja Rostov")
             )
  )+
  xlab("chapter")+
  geom_vline(xintercept = c(68,167,265),
             colour = "black",
             linetype = "dashed")


# Count weigthed for composition of the chapter by total sentiment
wp_pair_count_chapter %>% 
  filter(item1 %in% top3) %>% 
  filter(sentiment %in% c("positive","joy","fear","sadness")) %>%
  spread(sentiment, n, fill = 0) %>% 
  filter(CH <340) %>% 
  group_by(CH,item1) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  gather(sentiment, value,fear:sadness) %>% 
  ungroup() %>% 
  group_by(sentiment) %>% 
  arrange(CH) %>% 
  mutate(val.perc = value/sum(value)) %>% # divido per il numero totale di occorrenze, tengo conto dell'impatto 
  ungroup() %>% 
  group_by(CH,item1) %>% 
  mutate(perc = val.perc/sum(val.perc)) %>% # divido per le occorrenze totali che avvengono nel capitolo
  ggplot(aes(CH, perc, group = sentiment))+
  geom_smooth(aes(col = sentiment), se = F,
              span =.7)+
  facet_wrap(~item1, scales = "free_y", ncol = 3,
             labeller = labeller(item1 = c("andrew" = "Andrej Bolkonskij",
                                           "pierre" =  "Pierre Bezuchov",
                                           "natásha" = "Natalja Rostov")
             )
  )+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept = c(68,167,265),
             colour = "black",
             linetype = "dashed")+
  theme_minimal()+
  scale_colour_manual(values = c(
    fear = "#F8766D",
    positive = "#00BFC4",
    sadness = "#619CFF", 
    joy = "#F5C710"),
    name = NULL)+
  theme(
    strip.text.x = element_text(size = 20)
  )
  
  
############################################################################
############################################################################
###                                                                      ###
###                            TOPIC ANALYSIS                            ###
###                                                                      ###
############################################################################
############################################################################  


# Document-Topic Prob


# https://rpubs.com/MNidhi/NumberoftopicsLDA
sampling <- sample(1:365, replace = FALSE,size = nrow(dtm)*0.8 )

DTM_train <- dtm[sampling,]
DTM_test <- dtm[-sampling,]

##plot the metrics to get number of topics
system.time({
  tunes <- FindTopicsNumber(
    dtm = DTM_train,
    topics = c(5,6,7,8,9,10),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),
    method = "Gibbs",
    control = list(seed = 12345),
    mc.cores = 4L,
    verbose = TRUE
  )
})
FindTopicsNumber_plot(tunes)
#I'll go with 8

set.seed(seed = 234)
wp_lda <- LDA(dtm, k = 8)
# load("lda.RData")
wp_beta <- tidy(wp_lda, matrix = "beta")
wp_gamma <- tidy(wp_lda, matrix = "gamma") 


wp_gamma %>% 
  mutate(document = as.numeric(document)) %>% 
  group_by(document) %>% 
  arrange(document) %>% 
  top_n(1,gamma) %>% 
  ggplot(aes(document, gamma, group = topic, col = factor(topic)))+
  # geom_point()+
  geom_smooth(se = F, span = 0.8, show.legend = F)+
  facet_wrap(~topic,scales = "free_y",
             labeller = labeller( topic = c(
               "1" = "Battle Description",
               "2" = "  French Invasion",
               "3" = "Russian Society",
               "4" = "Rostov Story",
               "5" = "Andrej Story",
               "6" = "History and Time",
               "7" = "Pierre Story",
               "8" = "Charachters in Battle"
             )))+
  geom_vline(xintercept = c(68,167,265,339),
             colour = "red",
             linetype = "dashed")+
  theme(
    panel.background = element_rect(fill = NA,  colour = "grey20"),
    panel.grid = element_line(colour = "grey90")
  )+
  ylab(NULL)+
  xlab(NULL)

  
wp_beta %>% 
  group_by(topic) %>% 
  top_n(15,beta) %>% 
  ungroup %>% 
  ggplot(aes(reorder_within(term, beta, topic,sep = " t."), beta, fill = factor(topic)))+
  geom_col(show.legend = F)+
  coord_flip()+
  facet_wrap(~topic,scales = "free",
             labeller = labeller( topic = c(
               "1" = "Battle Description",
               "2" = "French Invasion",
               "3" = "Russian Society",
               "4" = "Rostov Story",
               "5" = "Andrej Story",
               "6" = "History and Time",
               "7" = "Pierre Story",
               "8" = "Charachters in Battle"
             )))+
  theme(
    panel.background = element_rect(fill = NA,  colour = "grey20"),
    panel.grid = element_line(colour = "grey90")
  )+
  ylab(NULL)+
  xlab(NULL)+
  theme(
    axis.text.y = element_text(size = 10)
  )


  
# Gamma Probabilities per Volume ####
topics <- c(
  "Battle Description",
  "French Invasion",
  "Russian Society",
  "Rostov Story",
  "Andrej Story",
  "History and Time",
  "Pierre Story",
  "Charachters in Battle"
)
  
# Looking at most common topic by chapter, book and volume
  
wp_gamma_named <- wp_gamma
wp_gamma_named$topic <- factor(wp_gamma$topic,
                               levels = 1:8,
                               labels = topics)
wp_gamma_named %>% 
  mutate(document = as.numeric(document)) %>% 
  group_by(document) %>% 
  arrange(document) %>% 
  top_n(1,gamma) %>% 
  ggplot(aes(document, fill = topic)) +
  geom_bar(position = "dodge", show.legend = F)+
  theme_minimal()+
  scale_fill_nejm()+
  ylab(NULL)+
  xlab(NULL)+
  theme(
    legend.key.height = unit(1.3, 'cm'), 
    legend.key.width = unit(1, 'cm'),
    legend.text = element_text(size=15),
    legend.title = element_blank()
  )+
  geom_vline(
    xintercept = c(68.5, 167.5,265.5,339.5),
    size = 1.5, linetype = "dashed"
  )


wp_gamma_named %>% 
  mutate(document = as.numeric(document)) %>% 
  group_by(document) %>% 
  arrange(document) %>% 
  inner_join(warpeace, by = c(document = "chapter")) %>% 
  select(-text, -volume) %>% 
  top_n(1, gamma) %>% 
  group_by(book, topic) %>% 
  count(topic) %>% 
  filter(n>3) %>% 
  group_by(book) %>% 
  mutate(topic.perc = n/sum(n)) %>% 
  arrange(book, -topic.perc) %>% 
  ggplot(aes(book,topic.perc, fill = topic)) +
  geom_col(position = "stack", show.legend = F)+
  theme_minimal()+
  scale_fill_nejm()+
  ylab(NULL)+
  xlab(NULL)+
  theme(
    legend.key.height = unit(1.3, 'cm'), 
    legend.key.width = unit(1, 'cm'),
    legend.text = element_text(size=15),
    legend.title = element_blank()
  )+
  geom_vline(xintercept = c(3.49,8.49,11.49,15.49),
             size = 1.1, linetype = "dashed")


wp_gamma_named %>% 
  mutate(document = as.numeric(document)) %>% 
  group_by(document) %>% 
  arrange(document) %>% 
  inner_join(warpeace, by = c(document = "chapter")) %>% 
  select(-text, -book) %>% 
  top_n(1, gamma) %>% 
  group_by(volume, topic) %>% 
  count(topic) %>% 
  filter(n>6) %>% 
  group_by(volume) %>% 
  mutate(topic.perc = n/sum(n)) %>% 
  arrange(volume, -topic.perc) %>% 
  ggplot(aes(volume,topic.perc, fill = topic)) +
  geom_col(position = "stack")+
  scale_fill_nejm()+
  ylab(NULL)+
  xlab(NULL)+
  theme_minimal()+
  theme(
    legend.key.height = unit(1.3, 'cm'), 
    legend.key.width = unit(1, 'cm'),
    legend.text = element_text(size=25),
    legend.title = element_blank()
  )
  

# Topic Boxplot by volume
wp_gamma_named %>% 
  mutate(document = as.numeric(document)) %>% 
  group_by(document) %>% 
  arrange(document) %>% 
  inner_join(warpeace, by = c(document = "chapter")) %>% 
  ggplot(aes(topic, gamma, fill = topic, alpha = 0.3))+
  geom_jitter(aes(fill = topic, col = topic),
            show.legend = F)+
  geom_boxplot(
    show.legend = F,
    outlier.shape = NA
  )+
  facet_wrap(~volume, labeller= labeller(.cols = c(
    "1" = "volume 1",
    "2" = "volume 2",
    "3" = "volume 3",
    "4" = "volume 4",
    "5" = "epilogues"
  )))+
  theme_minimal()+
  xlab(NULL)+
  ylab(NULL)+
  theme(
    panel.background = element_rect(fill = NA,  colour = "grey20"),
    panel.grid = element_line(colour = "grey90"),
    axis.text.x = element_text(angle = -40, hjust = -0)
    )

# Top tf_idf by topic
wp_gamma %>% 
  mutate(document = as.numeric(document)) %>% 
  group_by(document) %>% 
  arrange(document) %>% 
  inner_join(wptoken, by = c(document = "chapter"))%>%
  group_by(document) %>%
  top_n(1,gamma) %>% 
  ungroup %>%
  group_by(topic) %>% 
  count(word, document) %>%  
  bind_tf_idf(word, topic, n) %>% 
  arrange(-tf_idf) %>% 
  top_n(7, tf_idf) %>% 
  ggplot(aes(reorder_within(word, n,topic, sep = " t."),n, fill = factor(topic)))+
  geom_col(show.legend = F)+
  facet_wrap(~topic,scales = "free",
             labeller = labeller( topic = c(
               "1" = "Battle Description",
               "2" = "French Invasion",
               "3" = "Russian Society",
               "4" = "Rostov Story",
               "5" = "Andrej Story",
               "6" = "History and Time",
               "7" = "Pierre Story",
               "8" = "Charachters in Battle"
             )))+
  coord_flip()

# Isnt really intering
  
# Topic Sentiment ####
  
wp_beta_named  <- wp_beta
wp_beta_named$topic <- factor(wp_beta$topic,
                               levels = 1:8,
                               labels = topics)
  
  
wp_beta %>% 
    group_by(topic) %>% 
    top_n(300,beta) %>% 
    arrange(topic) %>% 
    inner_join(get_sentiments("nrc"), by = c(term = "word")) %>% 
    group_by(topic) %>%
    count(sentiment) %>% 
    top_n(4, n) %>% 
    ggplot(aes(reorder_within(sentiment, n, topic, sep = " t."),
               n, fill = factor(topic)))+
    geom_bar(stat = "identity",
             show.legend = F)+
    scale_x_reordered()+
    coord_flip()+
    facet_wrap(~topic, scales = "free_y",
               labeller = labeller( topic = c(
                 "1" = "Battle Description",
                 "2" = "French Invasion",
                 "3" = "Russian Society",
                 "4" = "Rostov Story",
                 "5" = "Andrej Story",
                 "6" = "History and Time",
                 "7" = "Pierre Story",
                 "8" = "Charachters in Battle"
               )))+
  theme_minimal()+
  xlab(NULL)+
  ylab(NULL)+
  theme(
    panel.background = element_rect(fill = NA,  colour = "grey20"),
    panel.grid = element_line(colour = "grey90"),
    axis.text.x = element_text(angle = -40, hjust = -0)
  )
  
  
wp_beta %>% 
    group_by(topic) %>% 
    top_n(300,beta) %>% 
    arrange(topic) %>% 
    inner_join(get_sentiments("nrc"), by = c(term = "word")) %>% 
    group_by(topic) %>%
    count(sentiment) %>% 
    ungroup %>% 
    group_by(sentiment) %>% 
    mutate(sent.perc = n/sum(n)) %>% 
    ungroup %>% 
    group_by(topic) %>% 
    top_n(4, sent.perc) %>% 
    ggplot(aes(reorder_within(sentiment, sent.perc, topic, sep = " t."),
               sent.perc, fill = factor(topic)))+
    geom_bar(stat = "identity",
             show.legend = F)+
    scale_x_reordered()+
    coord_flip()+
    facet_wrap(~topic, scales = "free_y",
               labeller = labeller( topic = c(
                 "1" = "Battle Description",
                 "2" = "French Invasion",
                 "3" = "Russian Society",
                 "4" = "Rostov Story",
                 "5" = "Andrej Story",
                 "6" = "History and Time",
                 "7" = "Pierre Story",
                 "8" = "Charachters in Battle"
               )))+
  theme_minimal()+
  xlab(NULL)+
  ylab(NULL)+
  theme(
    panel.background = element_rect(fill = NA,  colour = "grey20"),
    panel.grid = element_line(colour = "grey90")
  )
  
# Which topic contributes more to a certain sentiment ####


wp_beta_named %>% 
  group_by(topic) %>% 
  top_n(500,beta) %>% 
  arrange(topic) %>% 
  inner_join(get_sentiments("nrc"), by = c(term = "word")) %>% 
  group_by(sentiment) %>%
  count(topic) %>% 
  top_n(4, n) %>% 
  mutate(sent1 = str_sub(sentiment, end = 3)) %>% 
  ggplot(aes(reorder_within(topic, n, sent1, sep = " "),
             n, fill = factor(sentiment)))+
  geom_bar(stat = "identity",
           show.legend = F)+
  scale_x_reordered()+
  coord_flip()+
  facet_wrap(~sentiment, scales = "free_y", ncol = 2)+
  theme_minimal()+
  xlab(NULL)+
  ylab(NULL)+
  theme(
    panel.background = element_rect(fill = NA,  colour = "grey20"),
    panel.grid = element_line(colour = "grey90")
    )



# Sentiment Changes during Chapters ####
# Let's look at some distribution of sentiment inside the topic

mains_topic <- c(
  "Rostov Story",
  "Andrej Story",
  "Pierre Story"
)
  

wptoken %>% 
  group_by(volume,book,chapter) %>% 
  count(word) %>% 
  filter(volume != "epilogues") %>% 
  inner_join(wp_beta_named, by = c(word = "term")) %>% 
  inner_join(get_sentiments("nrc")) %>% 
  filter(topic %in% mains_topic) %>% 
  filter(beta >0.0025) %>%
  filter(n>1.25) %>% 
  filter(sentiment %in% c(
    "fear",
    "joy",
    "trust",
    "surprise"
  )) %>% 
  group_by(topic, book, sentiment) %>% 
  count(sentiment) %>% 
  ungroup %>% 
  group_by(topic,book) %>% 
  mutate(tot.sents = sum(n)) %>% 
  mutate(sent.perc = n/tot.sents) %>% 
  ungroup %>% 
  ggplot(aes(book, sent.perc, col = sentiment))+
  geom_smooth(se = F, size = 1.5, span = 0.7) +
  facet_wrap(~topic, scales = "free_y", ncol = 1)+
  geom_vline(xintercept = c(3,8,11),
             colour = "red",
             linetype = "dashed", size =1)+
  theme_minimal()+
  xlab(NULL)+
  ylab(NULL)+
  theme(
    panel.background = element_rect(fill = NA,  colour = "grey20"),
    panel.grid = element_line(colour = "grey90")
    )


top_vol_top <- wp_gamma %>% 
  mutate(document = as.numeric(document)) %>% 
  group_by(document) %>% 
  arrange(document) %>% 
  inner_join(warpeace, by = c(document = "chapter")) %>% 
  select(-text) %>% 
  ungroup() %>% 
  filter(gamma>0.15) %>% 
  mutate(topic= factor(topic,
                       levels = 1:8,
                       labels = topics)) %>% 
  group_by(volume) %>% 
  count(topic) %>% 
  group_by(topic) %>% 
  slice_max(n)

table5 <- wp_gamma %>% 
  mutate(document = as.numeric(document)) %>% 
  group_by(document) %>% 
  arrange(document) %>% 
  inner_join(warpeace, by = c(document = "chapter")) %>% 
  select(-text) %>% 
  ungroup() %>% 
  filter(gamma>0.15) %>% 
  mutate(topic= factor(topic,
                       levels = 1:8,
                       labels = topics)) %>% 
  group_by(volume) %>% 
  count(topic) %>% 
  group_by(topic) %>% 
  summarize(tot = sum(n),
            perc.tot = paste(round(tot/365*100,1),"%"),
    max= max(n),
    ) %>% 
    inner_join(top_vol_top) %>% 
  select(-n) %>% 
  mutate(volume = round(volume,0)) %>% 
arrange(-tot)
  
print(xtable(table5))
  


###########################################################################
###########################################################################
###                                                                     ###
###                         SUPERVISED LEARNING                         ###
###                                                                     ###
###########################################################################
###########################################################################
  
  
# Cluster for charchters ######
wp_pair_count <- wptoken %>% 
  pairwise_count(word, chapter) 

chars <- c ("pierre", "natásha", "andrew", "nicholas", "mary",
            "borís", "kutúzov" , "napoleon","vasíli","pétya",
            "bagratión" , "bourienne" , "hippolyte", "denísov",
            "anatole", "sónya","hélène")    



char_tibble <- wp_pair_count %>% 
  filter( item1 %in% chars) %>% 
  filter(n>10) %>% 
  spread(item2, n, fill = 0)

char_matrix <- as.matrix(char_tibble[,-1], rownames.force = T) 
rownames(char_matrix) <- sort(chars)

# Hclustering 
char_dist <- daisy(char_matrix)
char_hclust <- hclust(char_dist, method = "ward.D2")

# Plotting index for optimal cluster number
criteria = list()
criteria$silhouette <- NbClust(char_matrix,method = "ward.D2",index = "silhouette", max.nc = 8)$All.index
criteria$dunn <- NbClust(char_matrix,method = "ward.D2",index = "dunn",max.nc = 8)$All.index

par(mfrow = c(1,2))
plot(criteria$silhouette, x = 2:8,type = "line", xlab = "")
+abline(v = which.max(criteria$silhouette[])+1, col = "red", lty = "dashed")
plot(criteria$dunn, x = 2:8,type = "line", xlab = "")
+ abline(v = which.max(criteria$dunn)+1, col = "red", lty = "dashed")
par(mfrow = c(1,1))

plot(char_hclust)+
  rect.hclust(char_hclust, k = 4)


char_tibble %>% 
  mutate(clust = cutree(char_hclust, k = 4)) %>% 
  relocate(clust, .after = item1) %>% 
  gather( "word", "n", à:youth) %>% 
  filter(!word %in% chars) %>% 
  filter(n>30) %>% 
  mutate(word = as.character(word)) %>% 
  mutate(clust = factor(clust, levels = c(1,2,3,4))) %>% 
  group_by(clust,word) %>% 
  summarize(tot = sum(n)/n()) %>% 
  top_n(6) %>% 
  ggplot(aes(reorder_within(word,tot,clust, sep = " c."), tot, fill= clust))+
  geom_col(show.legend = F)+
  coord_flip()+
  facet_wrap(~clust,scales = "free",
             labeller = labeller(clust = c(
               "1"="Russian Society",
               "2"="Andrej and Pierre",
               "3"="Generals",
               "4"="Rostovs"
             )))+
  theme_minimal()+
  xlab(NULL)+
  ylab(NULL)
  

char_tibble %>% 
  mutate(clust = cutree(char_hclust, k = 4)) %>% 
  relocate(clust, .after = item1) %>% 
  gather( "word", "n", à:youth) %>% 
  filter(!word %in% chars) %>% 
  filter(n>30) %>% 
  mutate(word = as.character(word)) %>% 
  mutate(clust = factor(clust, levels = c(1,2,3,4))) %>% 
  group_by(clust,word) %>%
  bind_tf_idf(word, item1, n) %>% 
  summarize(m_tf_idf = mean(tf_idf),
            tot = sum(n)/n()) %>%
  group_by(clust) %>% 
  top_n(6, m_tf_idf) %>% 
  ggplot(aes(reorder_within(word,tot,clust, sep = " c."), tot, fill= clust))+
  geom_col(show.legend = F)+
  coord_flip()+
  facet_wrap(~clust,scales = "free",
             labeller = labeller(clust = c(
               "1"="Russian Society",
               "2"="Andrej and Pierre",
               "3"="Generals",
               "4"="Rostovs"
             )))+
  theme_minimal()+
  xlab(NULL)+
  ylab(NULL)


cluster_char <- cutree(char_hclust, k = 4) %>%
  as_tibble() %>% 
  mutate(item1 = sort(chars)) %>% 
  rename(cluster = value)


freq_char <- colSums(char_matrix)
freq_char <- sort(freq_char, decreasing=TRUE)
mots <- freq_char[freq_char>500]
s <- char_matrix[1,which(colnames(char_matrix) %in% names(mots))]
for(i in 2:nrow(char_matrix)){
   s=cbind(s,char_matrix[i,which(colnames(char_matrix) %in% names(mots))])
}
colnames(s) <- chars
PCA(s)



# Cluster Analysis for topics ####
dtm
dtm.sparse <- removeSparseTerms(dtm, 0.825)
dtm.matrix <- as.matrix(dtm.sparse)

# HCLUST ####
# # For Chapters 
# wp_dist <- daisy(dtm.matrix)
# 
# nh <- NbClust(dtm.matrix, distance="euclidean", min.nc=3, max.nc=8, method="ward.D2")
# wp_clust <- hclust(wp_dist, method = "ward.D2")
# plot(wp_clust)+
#   rect.hclust(wp_clust, k = 6)
# 
# 
# clust_opt <- cutree(wp_clust,k = 6)
# 
# clust_opt <- clust_opt %>% 
#   as_tibble() %>% 
#   rename(cluster = value) %>% 
#   mutate(document = as.character(1:367))
# 
# 
# 
# # Size of the cluster
# clust_opt %>% 
#   group_by(cluster) %>% 
#   summarize(n=n()) %>% 
#   ggplot(aes(reorder(cluster,n),n, fill= factor(cluster)))+
#   geom_col(show.legend = F)+
#   coord_flip()+
#   theme_minimal()
# 
# table6 <- sort(-table(clust_opt$cluster))
# print(xtable(table6))
# 
# hclust_centers <- dtm.matrix %>%
#   as_tibble() %>% 
#   mutate(cluster = clust_opt$cluster) %>% 
#   gather("word", "n",added:rode ) %>% 
#   group_by(cluster, word) %>% 
#   summarize(m = mean(n)) %>% 
#   spread(word, m, fill = 0) %>%
#   ungroup %>% 
#   select(-cluster)


# KMEANS ####
nk <- NbClust(dtm.matrix, distance="euclidean", min.nc=3, max.nc=8, method="kmeans")

wp_kmeans <- kmeans(dtm.matrix, centers =  7)

kmean_clust <- wp_kmeans$cluster %>% 
  as_tibble() %>% 
  rename(cluster = value) %>% 
  mutate(document = as.character(1:367))

barplot(table(wp_kmeans$cluster), 
        main = "barplot for k-means clustering")

table7 <- sort(-table(wp_kmeans$cluster))
print(xtable(table7))

plotcluster(cmdscale(dist(dtm)), wp_kmeans$cluster)

kmeans.proto <- t(cl_prototypes(wp_kmeans))

par(mar = c(0, 0, 0, 0))
comparison.cloud(kmeans.proto, max.words = 600, scale=c(3,1.2,0.8))
par(mar = c(5, 4, 4, 1))


# CLUSTERS SENTIMENTS ####

# Topic of the KMEANS ####
wp_gamma_named %>% 
  inner_join(kmean_clust) %>% 
  mutate(document = as.numeric(document)) %>% 
  group_by(document) %>% 
  arrange(document) %>% 
  ggplot(aes(topic, gamma, fill = topic, alpha = 0.8))+
  geom_jitter(aes(fill = topic, col = topic),
              show.legend = F)+
  geom_boxplot(
    show.legend = F,
    outlier.shape = NA
  )+
  facet_wrap(~cluster, labeller = labeller(cluster = c(
    "1"="cluster 1 - General",
    "2"="cluster 2 - Natasha",
    "3"="cluster 3 - Family",
    "4"="cluster 4 - Andrej",
    "5"="cluster 5 - Napoleon",
    "6"="cluster 6 - Rostov",
    "7"="cluster 7 - Pierre"
  )))+
  theme(
    panel.background = element_rect(fill = NA,  colour = "grey20"),
    panel.grid = element_line(colour = "grey90"),
    axis.text.x = element_text(angle = -40, hjust = -0)
  )+
  scale_fill_nejm()+
  scale_color_nejm()+
  xlab(NULL)+
  ylab(NULL)


wp_gamma_named %>% 
  inner_join(kmean_clust) %>% 
  mutate(document = as.numeric(document)) %>% 
  arrange(document) %>% 
  filter(gamma>0.2) %>% 
  group_by(topic, cluster) %>% 
  summarize(n=n()) %>% 
  arrange(-cluster) %>% 
  group_by(cluster) %>% 
  mutate(perc = n/sum(n)) %>% 
  ggplot(aes(cluster, perc, fill = topic))+
  geom_col(position = "stack")+
  theme(
    panel.background = element_rect(fill = NA,  colour = "grey20"),
    panel.grid = element_line(colour = "grey90")
    )+
  scale_fill_nejm()+
  xlab(NULL)+
  ylab(NULL)


table8 <- kmean_clust %>% 
  mutate(document = as.numeric(document)) %>% 
  inner_join(warpeace, by = c(document = "chapter")) %>% 
  select(-text) %>% 
  group_by(volume) %>% 
  count(cluster) %>% 
  mutate(n = paste(round(n/sum(n)*100,0),"%")) %>% 
  spread(cluster, n, fill = 0) 

print(xtable(table8))

# Top words of the KMEANS ####
wptoken %>%
  mutate(chapter = as.character(chapter)) %>% 
  inner_join(kmean_clust, by = c(chapter = "document")) %>% 
  group_by(cluster) %>% 
  count(word) %>% 
  top_n(10) %>% 
  ggplot(aes(reorder_within(word, n,cluster, sep = " c."),n, fill = factor(cluster)))+
  geom_col(show.legend = F)+
  facet_wrap(~cluster,scales = "free_y",
             labeller = labeller(cluster = c(
    "1"="cluster 1",
    "2"="cluster 2",
    "3"="cluster 3",
    "4"="cluster 4",
    "5"="cluster 5",
    "6"="cluster 6",
    "7"="cluster 7"
  )))+
  coord_flip()+
  xlab(NULL)+
  ylab(NULL)+
  theme(
    panel.background = element_rect(fill = NA,  colour = "grey20"),
    panel.grid = element_line(colour = "grey90")  )


# Top discriminating word of HCLUST ####
wptoken %>%
  mutate(chapter = as.character(chapter)) %>% 
  inner_join(clust_opt, by = c(chapter = "document")) %>% 
  group_by(cluster) %>%
  count(word) %>% 
  bind_tf_idf(word, cluster, n) %>% 
  arrange(-tf_idf) %>% 
  top_n(7, tf_idf) %>% 
  ggplot(aes(reorder_within(word, n,cluster, sep = " c."),n, fill = factor(cluster)))+
  geom_col(show.legend = F)+
  facet_wrap(~cluster,scales = "free",
             labeller = labeller(cluster = c(
    "1"="cluster 1",
    "2"="cluster 2",
    "3"="cluster 3",
    "4"="cluster 4",
    "5"="cluster 5",
    "6"="cluster 6"
  )))+
  coord_flip()+
  xlab(NULL)+
  ylab(NULL)+
  theme(
    panel.background = element_rect(fill = NA,  colour = "grey20"),
    panel.grid = element_line(colour = "grey90")
  )

# Top discriminating word of KMEANS ####
wptoken %>%
  mutate(chapter = as.character(chapter)) %>% 
  inner_join(kmean_clust, by = c(chapter = "document")) %>% 
  group_by(cluster) %>%
  count(word) %>% 
  bind_tf_idf(word, cluster, n) %>% 
  arrange(-tf_idf) %>% 
  top_n(7, tf_idf) %>% 
  ggplot(aes(reorder_within(word, n,cluster, sep = " c."),n, fill = factor(cluster)))+
  geom_col(show.legend = F)+
  facet_wrap(~cluster,scales = "free",
             labeller = labeller(cluster = c(
    "1"="cluster 1",
    "2"="cluster 2",
    "3"="cluster 3",
    "4"="cluster 4",
    "5"="cluster 5",
    "6"="cluster 6",
    "7"="cluster 7"
  )))+
  coord_flip()+
  xlab(NULL)+
  ylab(NULL)+
  theme(
    panel.background = element_rect(fill = NA,  colour = "grey20"),
    panel.grid = element_line(colour = "grey90"))
  