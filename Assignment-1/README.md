# Data Science Assignment #1

Is news a downer? 'Hard' news is often thought to be about 'bad' things in the world. We will test this hypothesis using data from Twitter. 

1. Extract Tweets from news sources:  
Using the Twitter API, I downloaded 6000 tweets from 10 media sources.  I selected the media sources above so that it would cover country-wide perspective as well as global perspectives.  News tweets from each new source twitter handle was aggregated into one JSON file, which was then parsed into words so that I can run the sentiment analysis on them.  Here are the list of media sources that I used:
    1. WASHINGTON POST
    2. NYTIMES
    3. WALL STREET JOURNAL
    4. BBC NEWS WORLD
    5. CHICAGO TRIBUNE
    6. LA TIMES
    7. ALJAZEERA
    8. CCTV
    9. CNN
    10. ABC

2. Sentiment Analysis
Based on Pablo's lecture, I used the lexicon of positive and negative words to run sentiment analysis.
Each word from the news tweets was compared against the words that were classified as positive and negative in the word list to determine the sentiment of each tweets.

3. Result:
The sentiment analysis was presented both in text and graph format.  Out of 6,000 tweets that were extracted from 10 media source, 23% of them were Positive, 28% of them were Negative, and 49% of them were neutral.  
While there are more negative tweets than positive tweets, the difference is only 5% point, so it doesn't provide a strong evidence of the news being a downer.  
At the same time, given the large portion of being neutral at 49%, the sentiment analysis algorithms can be improved to measure more accurate sentiments of the news tweets.

