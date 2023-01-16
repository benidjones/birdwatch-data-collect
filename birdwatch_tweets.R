library(tidyverse)
library(httr)
library(jsonlite)
library(data.table)

# personal allegedly "secret" token
bearer_token = "INSERT BEARER TOKEN"

##### FILL IN DATE
bw_date = "1218"

# read in the Birdwatch data
notes = fread(paste0("birdwatch_notes_", bw_date, ".tsv"), header = T, 
                   colClasses = c("tweetId" = "character", 
                                  "noteId" = "character")) 
# some mathematical calculations to get number of runs
runs = nrow(notes) %/% 100 + 1
final_run = nrow(notes) - ((runs - 1) * 100)
total_runs = nrow(notes) %/% 3000 + 1

# static query header
headers = c("Authorization" = sprintf('Bearer %s', bearer_token))

# loopdity do
limiter = 0 # ensures too many requests aren't made
final = data.frame() # stores the collected data

for (ii in 1:runs) {
  # makes the program sleep once the request limit is reached
  if (limiter == 300) {
    print("Limit reached, resting!")
    Sys.sleep(900)
    limiter = 0
  }
  # runs the requests in batches of 100
  if (ii < runs) {
    # get the ids for query
    tweet_ids = notes$tweetId[((ii - 1) * 100 + 1):(ii * 100)]
    ids_query = paste(tweet_ids, collapse = ",")
    params = list("ids" = ids_query, 
                  "tweet.fields" = "created_at,public_metrics,possibly_sensitive", 
                  "expansions" = "author_id", 
                  "user.fields" = "verified")
    
    # make the request
    request = GET("https://api.twitter.com/2/tweets/", 
                  add_headers(headers),
                  query = params)
    
    # organize the data
    request_content = content(request, as = "text")
    request_json = fromJSON(request_content, flatten = TRUE)
    request_df = as.data.frame(request_json$data) %>% 
      select(c("possibly_sensitive", "id", "author_id", "edit_history_tweet_ids", "text", "created_at", "public_metrics.retweet_count", "public_metrics.reply_count", "public_metrics.like_count", "public_metrics.quote_count"))
    
    # add it to the final, increment counter
    final = rbind(final, request_df)
    limiter = limiter + 1
  }
  # final run
  else {
    tweet_ids = notes$tweetId[((ii - 1) * 100 + 1):nrow(notes)]
    ids_query = paste(tweet_ids, collapse = ",")
    params = list("ids" = ids_query, 
                  "tweet.fields" = "created_at,public_metrics,possibly_sensitive", 
                  "expansions" = "author_id", 
                  "user.fields" = "verified")
    
    # make the request
    request = GET("https://api.twitter.com/2/tweets/", 
                  add_headers(headers),
                  query = params)
    
    # organize the data
    request_content = content(request, as = "text")
    request_json = fromJSON(request_content, flatten = TRUE)
    request_df = as.data.frame(request_json$data) %>% 
      select(c("possibly_sensitive", "id", "author_id", "edit_history_tweet_ids", "text", "created_at", "public_metrics.retweet_count", "public_metrics.reply_count", "public_metrics.like_count", "public_metrics.quote_count"))
    
    
    # add it to the final, increment counter
    final = rbind(final, request_df)
    limiter = limiter + 1
  }
}

find_edits = NULL
for (list in final$edit_history_tweet_ids) {
  find_edits = c(find_edits, length(list))
}

final = final %>%
  select(-"edit_history_tweet_ids")

final$edits = find_edits

write_csv(final, paste0("tweets_data_from_", Sys.Date(), "bw_", bw_date, ".csv"))
