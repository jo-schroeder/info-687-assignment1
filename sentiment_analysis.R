# Sentiment analysis package vingette referenced: https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html

library(htm2txt)
library(tidyverse)
library(tidytext)
library(data.table)
library(syuzhet)
library(stringr)
library(ggtext)

# Ingesting text data - NYPost has a popup so we can't scrape it directly
bbc_url <- 'https://www.bbc.com/news/world-us-canada-66089626'
guard_url <- 'https://www.theguardian.com/education/2023/aug/11/college-legacy-admissions-affirmative-action-democrats'
#nyp_url <- 'https://nypost.com/2023/07/04/harvard-sued-over-overwhelmingly-white-legacy-admissions'

bbc <- gettxt(bbc_url)
guard <- gettxt(guard_url)
nyp <- read_file("nyp.txt")

# Creating a document term matrix, using the syuzhet for sentiment analysis
sentiment_df <- tibble(doc = c(seq(1:3)), text = c(bbc, guard, nyp), document = c("bbc", "guard", "nyp"))
sentiment_paragraphs <- sentiment_df %>% unnest_tokens(word, text) %>% count(word, doc, sort = TRUE)
sentiment_words <- sentiment_df %>% unnest_tokens(word,text) %>% count(word,document, sort = TRUE)

docs <- c(bbc, guard, nyp)
doc_names <- c("BBC", "Guardian", "New York Post")
df <- list()

for(i in 1:length(docs)){
  s_v <- get_sentences(docs[i])
  s_v_sentiment <- get_sentiment(s_v, method="syuzhet") %>%
    # Percentage values helps us to compare across documents of different lengths
    get_percentage_values(bins = 10) %>% as.data.frame() %>% t()
  df[[i]] <- data.frame(s_v_sentiment)
}

df <- rbindlist(df, fill = T)

# Manipulating data for visualization, visualizing
df %>% t() %>% as.data.frame() %>% rownames_to_column() %>%
  mutate(time = str_remove(rowname, "X")) %>%
  dplyr::rename("British Broadcasting \n Corporation (BBC)" = V1,
         "The Guardian" = V2,
         "New York Post" = V3) %>% select(-rowname) %>%
  pivot_longer(cols = c("British Broadcasting \n Corporation (BBC)", "The Guardian", "New York Post")) %>%
  ggplot(aes(x = as.numeric(time), y = value, group = name, color = name)) +
  geom_line() +
  scale_x_continuous(breaks = c(1:10), labels = c(paste0(seq(10, 100, 10), "%"))) +
  theme_classic() +
  theme(plot.caption = ggtext::element_markdown(size = 12, hjust = 0),
        text = element_text(size = 22)) +
  labs(color = "Article Source",
       x = "Narrative Time",
       y = "Average Sentiment",
       #title = "Comparing the Sentiment of Articles Covering the 2023 Harvard \n Legacy Admissions Lawsuit",
       caption =
       "**Interpreting Average Sentiment:** Average sentiment is the mean sentiment value of a given set of words. <br>
       e.g. A value of 0 represents an netural average word sentiment, <br> Positive values represent a positive average word sentiment, <br>
       Negative values represent a negative sentiment. <br>
       **Interpreting Narrative Time:** Narrative time standardizes average sentiment for comparison across articles of different lengths. <br>
       e.g. Narrative time of 10% captures the first 10% of an article, <br>
       Narrative time of 20% captures the second 10% (i.e. 10-20%) of the article.
       <br> Sentiment analyzed using the syuzhet R Package and the syuzhet method")

# Individual article summary statistics and visualization
for(i in 1:length(docs)){
  sentiment <- sentiment_words %>% filter(document == docs[i]) %>% pull(word)
  sentiment <- get_nrc_sentiment(docs[i])
  score <- sum(sentiment)
  sentiment <- sentiment %>% select(-positive, -negative) %>%
    pivot_longer(cols = everything()) %>%
    slice_max(value, n = 2)
  print(paste0("Document ", doc_names[i],
               ": Length: ", str_count(docs[i], "\\W+"),
               ": Score: ", score,
               " Dominant Sentiments: ", sentiment[1, 1], ", ", sentiment[2, 1]))
}

bbc_sentiment <- sentiment_words %>% filter(document == "bbc") %>% pull(word)
bbc_sentiment <- get_nrc_sentiment(bbc)
sum(bbc_sentiment)
s_v <- get_sentences(bbc)
s_v_sentiment <- get_sentiment(s_v)
plot(
  s_v_sentiment,
  type="l",
  main="BBC Article",
  xlab = "Narrative Time",
  ylab= "Emotional Valence"
)

guard_sentiment <- sentiment_words %>% filter(document == "guard") %>% pull(word)
guard_sentiment <- get_nrc_sentiment(guard)
sum(guard_sentiment)
s_v <- get_sentences(guard)
s_v_sentiment <- get_sentiment(s_v)
plot(
  s_v_sentiment,
  type="l",
  main="Guardian Article",
  xlab = "Narrative Time",
  ylab= "Emotional Valence"
)


nyp_sentiment <- sentiment_words %>% filter(document == "nyp") %>% pull(word)
nyp_sentiment <- get_nrc_sentiment(nyp)
sum(nyp_sentiment)
s_v <- get_sentences(nyp)
s_v_sentiment <- get_sentiment(s_v)
plot(
  s_v_sentiment,
  type="l",
  main="NYP Article",
  xlab = "Narrative Time",
  ylab= "Emotional Valence"
)

