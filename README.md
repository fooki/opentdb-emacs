# opentdb-emacs
An emacs package for creating a quiz using the [opentdb api](https://opentdb.com/).



## Running a quiz:
Running the following interactive command will open a new buffer with the quiz:

```
(opentdb-next-question)
```

![server-states](https://github.com/fooki/opentdb-emacs/blob/master/images/opentdb-quiz.gif?raw=true)


You can also fetch questions yourself and use them how you wish:
```
;; See opentdb.el for more functions
(opentdb-fetch-questions)
```

## Configuration:
The following custom fields can be changed:

### opentdb-difficulty
example: `(setq opentdb-difficulty "medium")`

Options:
- nil (for all)
- "easy"
- "medium"
- "hard"

### opentdb-type: 
example: `(setq opentdb-type "multiple")`

Options
- nil (for any type)
- "boolean" 
- "multiple" 

### opentdb-category
example `(setq opentdb-category "9")`

Options (note that this might be outdated, check the api):
- "9" - "General Knowledge"
- "10" - "Entertainment: Books"
- "11" - "Entertainment: Film"
- "12" - "Entertainment: Music"
- "13" - "Entertainment: Musicals & Theatres"
- "14" - "Entertainment: Television"
- "15" - "Entertainment: Video Games"
- "16" - "Entertainment: Board Games"
- "17" - "Science & Nature"
- "18" - "Science: Computers"
- "19" - "Science: Mathematics"
- "20" - "Mythology"
- "21" - "Sports"
- "22" - "Geography"
- "23" - "History"
- "24" - "Politics"
- "25" - "Art"
- "26" - "Celebrities"
- "27" - "Animals"
- "28" - "Vehicles"
- "29" - "Entertainment: Comics"
- "30" - "Science: Gadgets"
- "31" - "Entertainment: Japanese Anime & Manga"
- "32" - "Entertainment: Cartoon & Animations"
