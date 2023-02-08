#https://bookdown.org/ahn_media/bookdown-demo/cleantool.html#purrr
# Set Enviro --------------------------------------------------------------
rm(list=ls())
library(tidyverse)
library(conflicted)
library(elbird)
library(tidytext)
library(modelsummary)
library(epubr)
library(rvest)
library(wordcloud)

conflict_prefer('tokenize','elbird')
conflict_prefer('filter','dplyr')


# Introduction ------------------------------------------------------------



text_v <- c("배우며 제때에 실행하면 진실로 즐겁지 않겠는가?
            벗이 먼 곳에서부터 온다면 참으로 즐겁지 않겠는가?
            남이 알아주지 않아도 성내지 않는다면 참으로 군자답지 않겠는가?")
text_v
text_v_t<- tokenize(text_v)


text_df <- tibble(text = text_v)
text_df %>% unnest_tokens(output = word,input = text,token = "ngrams", n= 2)

text_v_t %>% count(form,sort = T) %>% 
  filter(n > 1) %>% 
  mutate(form = reorder(form,n)) %>% 
  ggplot(aes(form,n)) + 
  geom_col()+
  coord_flip()

# Exercise 1 --------------------------------------------------------------
rm(list=ls())

text <- tokenize('13인의 아해가 도로로 질주하오.
(길은 막다른 골목이 적당하오.)

제1의 아해가 무섭다고 그리오.
제2의 아해도 무섭다고 그리오.
제3의 아해도 무섭다고 그리오.
제4의 아해도 무섭다고 그리오.
제5의 아해도 무섭다고 그리오.
제6의 아해도 무섭다고 그리오.
제7의 아해도 무섭다고 그리오.
제8의 아해도 무섭다고 그리오.
제9의 아해도 무섭다고 그리오.
제10의 아해도 무섭다고 그리오.
제11의 아해가 무섭다고 그리오.
제12의 아해도 무섭다고 그리오.
제13의 아해도 무섭다고 그리오.
13인의 아해는 무서운 아해와 무서워하는 아해와 그렇게뿐이 모였소.
(다른 사정은 없는 것이 차라리 나았소)

그중에 1인의 아해가 무서운 아해라도 좋소.
그중에 2인의 아해가 무서운 아해라도 좋소.
그중에 2인의 아해가 무서워하는 아해라도 좋소.
그중에 1인의 아해가 무서워하는 아해라도 좋소.

(길은 뚫린 골목이라도 적당하오.)
13인의 아해가 도로로 질주하지 아니하여도 좋소.')

text %>% count(form,sort = T) %>% 
  filter(n > 3) %>% 
  mutate(form = reorder(form,n)) %>% 
  rename(words = form) %>% 
  ggplot(aes(words,n)) +
  geom_col() + 
  coord_flip()



# Exercise 2 --------------------------------------------------------------
rm(list=ls())


text <- '안희연의 시는 읽힌다. 읽을 기운을 포함해 살아갈 힘이 부족할 때도. 화려한 표현이나 어떤 반응을 자아내려는 장치의 피로를 배척하는 시. 의연한 태도, 맑고 풍성한 시어들에선 잘해보겠다는 마음을 내려놓을 때의 겸손도 느껴진다. 마주할 세상 앞에서 모든 턱을 낮추겠다는 마음이 먼저인 이에게 안희연의 글은 좋은 친구가 된다.
그가 세 번째 산문집 <단어의 집>을 펴냈다. “단어 생활자” 시인이 낱말로 세상을 흡수하는 모습을 볼 수 있는 첫 산문집이기도 하다.
45가지 단어가 소개된다. 버럭 아니고 “버력”. 광물이 들어 있지 않아 버려지는 허드레 돌을 뜻하는 단어로, 방파제 기초공사에 쓰이곤 한다. “이 세계가 광산이라면 신은 성실하게 인간 광물을 캐낼 것이다. 금인지 은인지, 버력인지 일단은 캐봐야 한다. 시작해봐야 알고, 끝나봐야 안다. 그러니까 나라는 인간의 최후를 미리부터 결론 내지 말고 일단은 나를 잘 다듬어가는 게 맞다. 적어도 내 삶을 버력의 자리에는 두지 않기 위해서.”
건물 하중을 견디는 구조체로, 맨 마지막까지 철거할 수 없는 벽을 가리키는 단어는 “내력벽”이다. 사람이 건물이라면 “모든 걸 부숴도 부서지지 않는 최후의 보루, 영혼의 핵심인 셈이니 그 자체로 의미 있고 아름다운 것”이다.
글을 쓰지 못하던 “무색무취무미한” 번아웃 시기에, 흔한 네온사인으로부터 글을 얻은 에피소드는 얼마나 반짝이는지. 색, 냄새, 맛이 없는 안정된 기체의 단어 “네온”. 네온사인을 만들려면 공기를 증류해 추출한 네온을 다시 유리관에 넣어 방전시키는 과정이 필요하다는 걸 알게 된 순간, 안희연은 “어김없이” 시를 발견한다.
“네온의 방전은 빛이 된대요. 방전에도 쓸모가 있어요. 그러니 방전되세요! 우리 일어나지 말아요. 이불 속에 그냥 있어요. 몸을 일으켜야 한다는 강박에 사로잡히지도 말아요. 멀리서 보면 네온사인처럼 보일 거예요. 꿈과 희망은 내일 날 밝으면 해요 우리.”
모든 단어는 시를 품고서 “몸에 붙”는다고 시인은 쓴다. 시가 우리를 덮은 피부라면, 영혼의 “따뜻한 움막”이라면. 단어의 집에 머무는 한 ‘끝’이란 말도 허무하지만은 않다. 똑같은 소리를 가진 단어 “끗”에서 시를 찾을 수 있으니. “끝이 아닌 끗의 자리에서, 끗과 함께, 한 끗 차이로도 완전히 뒤집히는 세계의 비밀을 예민하게 목격하는 자”로 살고 싶어진다.'

text <- tokenize(text)
text <- text %>% count(form,sort=T) %>% 
  filter(n > 3) %>% 
  mutate(form = reorder(form,n)) %>% 
  rename(words = form)
  
text %>% ggplot(aes(words,n)) + geom_col() + coord_flip()


# Binary File -------------------------------------------------------------
rm(list = ls())

download.file('https://www.dropbox.com/s/ug3pi74dnxb66wj/jikji.epub?dl=1',destfile = 'jikji.epub',mode='wb')

jikji_df <- epub('jikji.epub')
jikji_df %>% str()
glimpse(jikji_df)

df <- jikji_df$data[[1]]
df %>% glimpse()
tibble(text = df$text[1]) %>% 
  unnest_tokens(output = word, input = text,token = tokenize_tidy) %>% 
  count(word,sort = T) %>% 
  filter(n >= 20) %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(word,n)) +
  geom_col()+
  coord_flip()

# txt file ----------------------------------------------------------------
rm(list=ls())
moby_url <- 'https://www.gutenberg.org/files/2701/2701-0.txt'

moby_v <- read_lines('https://www.gutenberg.org/files/2701/2701-0.txt')
moby_v %>% head(3)

moby_scan <- scan(moby_url, what = "character")
moby_scan %>% head()

moby_scan_n <- scan(moby_url, what = "character", sep= "\n")
moby_scan_n %>% head()


# HTML --------------------------------------------------------------------
rm(list=ls())
moby_url_h <- 'https://www.gutenberg.org/files/2701/2701-h/2701-h.htm'
moby_html <- read_html(moby_url_h)
moby_html %>% html_node('body') %>% html_text()

# Exercise ----------------------------------------------------------------
rm(list=ls())

moby_url <- 'https://www.gutenberg.org/ebooks/2701.epub.noimages'
download.file(url = moby_url, 
              destfile = "moby.epub",
              mode="wb")
sense_url <- 'https://www.gutenberg.org/ebooks/161.epub.noimages'
download.file(url = sense_url, 
              destfile = "sense.epub",
              mode="wb")


moby <- epub('moby.epub')
sense <- epub('sense.epub')
moby %>% glimpse()
sense %>% glimpse()

moby_v <- moby$data[[1]]$text
sense_v <- sense$data[[1]]$text

tibble(text=moby_v) %>% unnest_tokens(output=word,input = text) %>% 
  count(word, sort = T) %>% 
  slice_max(n, n = 12) %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(word,n)) +
  geom_col() +
  coord_flip()

tibble(text = sense_v) %>% 
  unnest_tokens(output = word, input = text) %>% 
  count(word, sort = T) %>% 
  slice_max(n, n = 12) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot() + geom_col(aes(word, n)) +
  coord_flip() +
  labs(title = "Sense and Sensibility") +
  theme_light()

# Other exercise ----------------------------------------------------------
rm(list = ls())
jikji_df <- epub('jikji.epub')

text1 <- tokenize(jikji_df$data[[1]]$text[33])
text2 <- tokenize(jikji_df$data[[1]]$text[10])


text1 %>% count(form, sort= T) %>% 
  slice_max(n,n=12) %>% 
  rename(word = form) %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(word,n))+
  geom_col()+
  coord_flip()

text2 %>% count(form, sort= T) %>% 
  slice_max(n,n=12) %>% 
  rename(word = form) %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(word,n))+
  geom_col()+
  coord_flip()

# stringr -----------------------------------------------------------------
love_v <- c(
  "You still fascinate and inspire me. :)",
  "You influence me for the better ~ .",
  "You’re the object of my desire, the #1 Earthly reason for my existence!!!",
  " ... ",
  "그대는 여전히 매혹적이고 나에게 영감을 줍니다. ^^",
  "당신은 나로 하여금 더 나은 사람이 되도록 했습니다 ~.",
  "당신은 내 욕망의 대상이요, 내가 이 세상에 존재하는 첫번째 이유입니다!!!~",
  " "
)
tibble(text = love_v) %>% filter(!str_detect(string = text,pattern = '그대'))


tibble(text = love_v) %>% 
  unnest_tokens(output = word, input = text,token = tokenize_tidy) %>% 
  filter(str_detect(string = word, pattern = "그대"))

love_v %>% str_which(pattern = "You")
love_v %>% str_count(pattern = "You")

# stringr -----------------------------------------------------------------
rm(list=ls())

text <- '13인의 아해가 도로로 질주하오.
(길은 막다른 골목이 적당하오.)

제1의 아해가 무섭다고 그리오.
제2의 아해도 무섭다고 그리오.
제3의 아해도 무섭다고 그리오.
제4의 아해도 무섭다고 그리오.
제5의 아해도 무섭다고 그리오.
제6의 아해도 무섭다고 그리오.
제7의 아해도 무섭다고 그리오.
제8의 아해도 무섭다고 그리오.
제9의 아해도 무섭다고 그리오.
제10의 아해도 무섭다고 그리오.
제11의 아해가 무섭다고 그리오.
제12의 아해도 무섭다고 그리오.
제13의 아해도 무섭다고 그리오.
13인의 아해는 무서운 아해와 무서워하는 아해와 그렇게뿐이 모였소.
(다른 사정은 없는 것이 차라리 나았소)

그중에 1인의 아해가 무서운 아해라도 좋소.
그중에 2인의 아해가 무서운 아해라도 좋소.
그중에 2인의 아해가 무서워하는 아해라도 좋소.
그중에 1인의 아해가 무서워하는 아해라도 좋소.

(길은 뚫린 골목이라도 적당하오.)
13인의 아해가 도로로 질주하지 아니하여도 좋소.
'

text %>% str_squish() %>% str_extract_all(pattern = '\\w*\\d+\\w*') 
text %>% str_squish() %>% str_remove_all(pattern = '\\w*\\d+\\w*')
text %>% str_squish() %>% str_replace_all(pattern = '아해\\w*','아해')
text %>% str_squish() %>% str_extract_all(pattern = '질주\\w*')
text %>% str_squish() %>% str_replace_all(pattern = '질주\\w*','질주')
text %>% str_squish() %>% str_extract_all(pattern = '무\\w*')
text %>% str_squish() %>% str_replace_all(pattern = '무\\w*','무섭다')



ogamdo_clean_v <- text %>% str_squish() %>% str_remove_all(pattern = '\\w*\\d+\\w*')%>%
  str_replace_all(pattern = '아해\\w*','아해')%>%
  str_replace_all(pattern = '질주\\w*','질주')%>%
  str_replace_all(pattern = '무\\w*','무섭다')

ogamdo_clean_v %>% tokenize() %>%
  count(form, sort = T) %>%
  filter(str_count(form) > 1) %>%
  mutate(text = reorder(form,n)) %>% 
  ggplot(aes(text,n))+
  geom_col() +
  coord_flip()

# dplyr -------------------------------------------------------------------
rm(list=ls())

data("stop_words")  
glimpse(stop_words)
stop_words %>% distinct(lexicon)

stop_words %>% group_by(lexicon) %>% glimpse()
stop_words %>% summarise(wordN = n())
stop_words %>% group_by(lexicon) %>% summarise(wordN = n())
stop_words %>% count(word)
stop_words %>% summarise(n_distinct(lexicon))
stop_words %>% arrange(word, .by_group = F)
stop_words %>% arrange(word, .by_group = T)
stop_words %>% group_by(lexicon) %>% arrange(word, .by_group = T)


tibble(text = "You inspire me, You influence me for the better.") %>%
  unnest_tokens(word, text) %>%
  slice(-(2:4))


tibble(text = "You inspire me, You influence me for the better.") %>%
  unnest_tokens(word, text) %>%
  slice_head(prop = 0.5)


tibble(text = "You inspire me, You influence me for the better.") %>%
  unnest_tokens(word, text) %>%
  count(word) %>% 
  slice_max(n, n = 2)



tokenize("You inspire me, You influence me for the better.") %>%
  transmute(word = str_c(form, '/', tag),
            start = start,
            len = len)

1:4 %>% recode(`4` = 10L)
class(c(1.3, 2.5))
class(1:4)

tibble(text = "You inspire me, You influence me for the better.") %>%
  unnest_tokens(word, text) %>%
  mutate(recoded = recode(word,
                          inspire = "INSPIRE",
                          better = "BETTER"))


c("희", "노", "애", "락") %>% 
  str_detect("희|애|락") %>% 
  if_else("긍정","부정")


you_v <- c("you", "you'd", "you'll", "you're", "you've", "your", "yours", "yourself", "yourselves", "young", "younger", "youngest")

case_when(
  str_detect(you_v, ".*sel.*") ~ "self",
  str_detect(you_v, "\\byoung.*") ~ "young",
  str_detect(you_v, "\\byou.*") ~ "you",
)


a_df <- tribble(
  ~A, ~B, ~C,
  "a", "t", 1,
  "b", "u", 2,
  "CC", "V", 3,
)

b_df <- tribble(
  ~A, ~B, ~D,
  "a", "t", 3,
  "b", "u", 2,
  "DD", "W", 1,
)


bind_rows(dfa= a_df,dfb = b_df,.id = ('dfid'))


a_df <- tribble(
  ~A, ~B, ~C,
  "a", "t", 1,
  "b", "u", 2,
  "CC", "V", 3,
)

b_df <- tribble(
  ~A, ~B, ~C,
  "a", "t", 3,
  "b", "u", 2,
  "DD", "W", 1,
)
intersect(x = a_df, y = b_df) #교집합
setdiff(x = a_df, y = b_df) #a_df - b_df 차집합
union(x = a_df, y = b_df) #합집합
bind_cols(a_df, b_df)
anti_join(x = a_df, y = b_df) #왼쪽 df의 값을 보존(차집합과 비슷)
anti_join(x = a_df, y = b_df,by = 'A') #특정 열만을 기준으로 join
semi_join(a_df, b_df)
inner_join(x = a_df, y = b_df)
left_join(x = a_df, y = b_df)
full_join(x = a_df, y = b_df)



# Ex ----------------------------------------------------------------------
rm(list = ls())
sonnet27_v <- "Weary with toil I haste me to my bed,
The dear repose for limbs with travel tired;
But then begins a journey in my head
To work my mind when body's work's expired;
For then my thoughts, from far where I abide,
Intend a zealous pilgrimage to thee,
And keep my drooping eyelids open wide
Looking on darkness which the blind do see:
Save that my soul's imaginary sight
Presents thy shadow to my sightless view,
Which like a jewel hung in ghastly night
Makes black night beauteous and her old face new.
Lo! thus by day my limbs, by night my mind,
For thee, and for myself, no quietness find."

stop_words
tibble(text = sonnet27_v) %>% unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  count(word) %>% 
  mutate(word = reorder(word,n)) %>%
  slice_max(n,prop=0.2,with_ties = F) %>% 
  ggplot(aes(word,n)) + 
  geom_col()+
  coord_flip()


tibble(text = sonnet27_v) %>% unnest_tokens(word,text) %>% 
  inner_join(get_sentiments('afinn'),by = 'word') %>% 
  arrange(desc(value))


tibble(text = sonnet27_v) %>% unnest_tokens(word,text) %>% 
  inner_join(get_sentiments('bing'),by = 'word') %>% 
  arrange(desc(sentiment))

tibble(text = sonnet27_v) %>% unnest_tokens(word,text) %>% 
  inner_join(get_sentiments('nrc'),by = 'word',multiple = "all") %>% 
  arrange(desc(sentiment))

tibble(text = sonnet27_v) %>% unnest_tokens(word,text) %>% 
  inner_join(get_sentiments('loughran'),by = 'word') %>% 
  arrange(desc(sentiment))

sonnet27_tk <- tibble(text = sonnet27_v) %>% 
  unnest_tokens(word, text)

sonnet27_afinn <- sonnet27_tk %>% inner_join(get_sentiments("afinn"))
sonnet27_bing <- sonnet27_tk %>% inner_join(get_sentiments("bing")) 
sonnet27_nrc <- sonnet27_tk %>% inner_join(get_sentiments("nrc"),multiple = "all") 


inner_join(sonnet27_afinn, sonnet27_bing)
inner_join(sonnet27_afinn, sonnet27_nrc,multiple = 'all')
inner_join(sonnet27_bing, sonnet27_nrc, 
           by = "word", 
           suffix = c("_bing", "_nrc"))
inner_join(sonnet27_afinn, sonnet27_bing) %>% 
  inner_join(sonnet27_nrc, by = "word", suffix = c("_bing", "_nrc"))


full_dic <- full_join(sonnet27_afinn, sonnet27_bing) %>% 
  full_join(sonnet27_nrc, by = "word", suffix = c("_bing", "_nrc"))

tibble(text = sonnet27_v) %>% 
  unnest_tokens(word, text) %>% 
  inner_join(full_dic) %>% 
  count(word, sort = T) %>% 
  filter(n > 1) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot() + geom_col(aes(word, n)) + 
  coord_flip() + 
  ggtitle("Sonnet27 Emotion Words")



# Exercise sentiments -----------------------------------------------------
rm(list = ls())

url <- 'https://www.reuters.com/world/middle-east/turkey-quake-rescue-teams-comb-through-wreckage-iskenderun-hospital-2023-02-06/'
read_html(url) %>% html_node('#main-content > article > div.article__main__33WV2 > div.article__content__6hMn9 > div > div > div.article-body__content__17Yit.paywall-article') %>%
  html_text() %>% str_squish() %>% str_remove_all(pattern = '\\\\*') %>% 
  tibble(text = .) %>% 
  unnest_tokens(output = word,input = text) %>%
  count(word,sort = T) %>% 
  inner_join(get_sentiments('bing')) %>% 
  group_by(sentiment) %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(n,word,fill= sentiment)) + 
  geom_col()+
  facet_wrap(~sentiment,scales= 'free')+
  scale_x_continuous(breaks = c(0:4))
  


# Purrr -------------------------------------------------------------------
rm(list = ls())
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
df
df$a %>% mean()
df$b %>% mean()
df$c %>% mean()
df$d %>% mean()

df %>% map(mean) %>% flatten_df()

map(1:2,~rnorm(n =3 ,mean = .))
map2(1:2, 11:12, ~rnorm(n = 3, mean = ., sd = .))

arg_l <- list(1:2, 11:12,  21:22)
arg_l %>% pmap(rnorm)
arg_l %>% pmap(~rnorm(n = ., mean = ., sd = .))

arg_l <- list(mean = 11:12, n = 1:2, sd = 21:22)
arg_l %>% pmap(rnorm)
arg_l %>% pmap(~rnorm(n = ., mean = ., sd = .))


# Tidyr -------------------------------------------------------------------
rm(list = ls())

df <- tribble(
  ~model, ~`2000`, ~`2001`, ~`2002`,
  "A", 100, 300, 400,
  "B", 400, 500, 600,
  "C", 900, 1000, 1200
)
df

df %>% pivot_longer(cols = starts_with('2'),names_to = 'Year',values_to = 'case') %>% 
  pivot_wider(names_from = Year,values_from = case)

tb <- tibble(email = c(NA, "apple@ddd.com", "tesla@ttt.com", "amazon@rrr.com"), 
             status = str_c("g", "xxx", 1:4))
tb

tb %>% separate(col = email,into = c('id','comp','type'))
tb %>% separate(col = status, 
                into = c("s1", "s2"), 
                sep = "xxx")


tbr <- tibble(year = c(1999, 2000),
              email = c("apple@ddd.com", "tesla@ttt.com, amazon@rrr.com"),
              stat = c("1", "2,3"))
tbr
tbr %>% separate_rows(email,sep =', ' )



# Exercise ----------------------------------------------------------------
rm(list = ls())

sonnet27_v <- "Weary with toil I haste me to my bed,
The dear repose for limbs with travel tired;
But then begins a journey in my head
To work my mind when body's work's expired;
For then my thoughts, from far where I abide,
Intend a zealous pilgrimage to thee,
And keep my drooping eyelids open wide
Looking on darkness which the blind do see:
Save that my soul's imaginary sight
Presents thy shadow to my sightless view,
Which like a jewel hung in ghastly night
Makes black night beauteous and her old face new.
Lo! thus by day my limbs, by night my mind,
For thee, and for myself, no quietness find."

s27_bing <- tibble(text = sonnet27_v) %>% 
  unnest_tokens(word, text) %>% 
  inner_join(get_sentiments("bing"))

tibble(text = sonnet27_v) %>% 
  unnest_tokens(word, text) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(sentiment,sort = T)

s27_bing %>% pivot_wider(names_from = sentiment,values_from = word) %>% 
  flatten() %>% map_dfc(length) %>% 
  pivot_longer(1:2,names_to = 'sentiment', values_to = 'count') %>% 
  ggplot(aes(count,sentiment)) + 
  geom_col()

# regex -------------------------------------------------------------------

txt2_v <- c(
  "newline\n\n,tab\t\t,CR\r\r,FF\f\fVT\v\v,space  space",
  "abcABC가나다123구두점!@#%&*()_{}:;/?기타$^+<>`~",
  "가다aaaaapppP 8e.A pear... 123"
)
str_view_all(txt2_v, pattern = "s") 
str_view_all(txt2_v, pattern = "\\s") 
str_view_all(txt2_v, pattern = "\\d\\b")
str_view_all(txt2_v, pattern = "\\b")
str_view_all(txt2_v, pattern = ".")
str_view_all(txt2_v, ".e.")
str_view_all(txt2_v, "\\..") 
str_view_all(txt2_v, "\\.\\.") 
str_view_all(txt2_v, "\\.") 
str_view_all(txt2_v, "\\w\\w\\w\\w\\w")
str_view_all(txt2_v, "\\w") 
str_view_all(txt2_v, "\\D") 
str_view_all(txt2_v, "[:punct:]") 
str_view_all(txt2_v, "[:graph:]")
str_view_all(txt2_v, "[aeiou]") 
str_view_all(txt2_v, "[^abcABC가나다]") 
str_view_all(txt2_v, "[a-e]") # "a" 부터 "e" 까지
c("apple", "eeeppp", "table") %>% 
  str_view_all("(ap|e)p") #“ap” 또는 “e”와 그 다음의 “p”

qt_v <- "aaaaaa, taaaaaa, ta, ttttaaaaa, ttttt"
str_view_all(qt_v, "ta{3}") 
str_view_all(qt_v, "ta{2,}")
str_view_all(qt_v, "ta+") 
str_view_all(qt_v, "ta*") 
str_view_all(qt_v, "ta{1,3}") 
str_view_all("You and I inFluence Him ~", "\\b[A-Z][a-z]+")
str_view_all("You influence me @@@ 123 ~~~ ^^ ㅋㅋㅋ ", "[^[:alnum:]]+" )


# Exercise ----------------------------------------------------------------

ssn_v <- c("900230-1234567, 901030-2234567", 
           "902230-3234567", 
           "901030-6234567, 901050-4234567")
str_view_all(ssn_v,'\\d{6}')
str_view_all(ssn_v,  "\\d{2}(0[1-9]|1[0-2])[0-3][0-9]-\\d{7}") 

email1_v <- c("apple@ddd.kr, tesla@sss.com, amazon@ttt.info, MS@x.com")
str_view_all(email1_v,'.+@.\\..+')

mu_v <- c("두렵고 무서움", "무서운", "무서운 꿈에", "무서워서", "무리야")
str_view_all(mu_v,'^무서')
str_view_all(mu_v,'\\b무서\\w*\\b') #\\b무서\\w*\\b -> 단어시작, '무서'로 시작하는 단어, 후에 오는 아무 단어, 단어 끝.
mu_v %>% str_subset(pattern = '무서')

tst_v <- c("I am, I, my, me, mine, I'll, I've, I'd, myself", 
           "i am, i, i'm, i've",
           "interesting, INCH, mean, myopia")
str_view_all(tst_v,'\\b(i|I)\'\\w*|\\b(i|I)\\s\\w*|\\b(i|I)\\b|\\bmy\\b|\\bmine|\\me\\b|\\bmyself')
tst_v %>% str_replace_all(pattern = '\\b(i|I)\'\\w*|\\b(i|I)\\s\\w*|\\b(i|I)\\b|\\bmy\\b|\\bmine|\\me\\b|\\bmysel\\w+\\b', 'me')

file_v <- c("file1999.txt", "file2000.txt", "file2001.txt", "file2002.txt",
            "file2003.txt", "file2004.txt", "file2005.txt", "file2006.txt")
file_v %>% str_view_all('[:alpha:]+2\\d{2}[1-9][:punct:]+\\w+')


# 감정분석 --------------------------------------------------------------------

url_v <- "https://github.com/park1200656/KnuSentiLex/archive/refs/heads/master.zip"
dest_v <- "/knusenti.zip"

download.file(url = url_v,
              destfile = 'knusenti.zip',
              mode = "wb")

list.files(getwd())

unzip('knusenti.zip')

list.files('KnuSentiLex-master/')

senti_name_v <- list.files("KnuSentiLex-master/.")[9]

senti_dic_df <- read_tsv(str_c("KnuSentiLex-master/", senti_name_v),col_names = F)
senti_dic_df <- senti_dic_df %>% rename(word = X1, sScore = X2)

senti_dic_df %>% filter(sScore == 2) %>% arrange(word)
senti_dic_df %>% 
  filter(!is.na(sScore)) %>% 
  add_row(word = "갈등", sScore = -1) -> senti_dic_df 

senti_dic_df<- senti_dic_df %>% 
   add_row(word = '귀엽', sScore = 2)

senti_dic_df<- senti_dic_df %>%
  mutate(sScore = replace(sScore, word == '원망', - 2))
  

senti_dic_df<- senti_dic_df %>%
  mutate(sScore = replace(sScore, word == '원통', - 2))

write_csv(senti_dic_df,file = 'sentimental_dic.csv')


con_v <- "유구한 역사와 전통에 빛나는 우리 대한국민은 3·1운동으로 건립된 대한민국임시정부의 법통과 불의에 항거한 4·19민주이념을 계승하고, 조국의 민주개혁과 평화적 통일의 사명에 입각하여 정의·인도와 동포애로써 민족의 단결을 공고히 하고, 모든 사회적 폐습과 불의를 타파하며, 자율과 조화를 바탕으로 자유민주적 기본질서를 더욱 확고히 하여 정치·경제·사회·문화의 모든 영역에 있어서 각인의 기회를 균등히 하고, 능력을 최고도로 발휘하게 하며, 자유와 권리에 따르는 책임과 의무를 완수하게 하여, 안으로는 국민생활의 균등한 향상을 기하고 밖으로는 항구적인 세계평화와 인류공영에 이바지함으로써 우리들과 우리들의 자손의 안전과 자유와 행복을 영원히 확보할 것을 다짐하면서 1948년 7월 12일에 제정되고 8차에 걸쳐 개정된 헌법을 이제 국회의 의결을 거쳐 국민투표에 의하여 개정한다."

con_tk <- tokenize(con_v)

con_tk %>% rename(word = form) %>%  inner_join(senti_dic_df) %>% summarise(tscore = sum(sScore))



# exercise ----------------------------------------------------------------
dir.create("data")
file_url <- 'https://www.dropbox.com/s/ug3pi74dnxb66wj/jikji.epub?dl=1'
download.file(url = file_url, 
              destfile = "jikji.epub",
              mode="wb")

jikji_all_df <- epub("jikji.epub") %>% .$data %>% .[[1]] %>% 
  arrange(desc(nchar))

sosul_v <- epub("jikji.epub") %>% .$data %>% .[[1]] %>% .$text
sosul_v %>% glimpse


title_v <- sosul_v %>% str_extract(".*\\b")
author_v <- sosul_v %>% str_extract("지은이.*\\b") %>% str_remove("지은이: ")
source_v <- sosul_v %>% str_extract("출전.*\\b") %>% 
  str_remove(":") %>% str_remove("\\)") %>% str_remove("출전 ")
main_v <- sosul_v %>% str_squish() %>% 
  str_extract("본문.*") %>% str_remove("본문|:")

sosul_df <- tibble(title = title_v, 
                   author = author_v,
                   source = source_v,
                   main = main_v) %>% .[1:89,]
sosul_df %>% glimpse()
sosul2_df <- sosul_df[9:10, ] 

sosul2_df %>% unnest_tokens(word, main, token = tokenize_tidy) %>% 
  separate_wider_delim(word,delim = '/',names = c('word','type')) %>% 
  inner_join(senti_dic_df) %>% 
  group_by(author) %>% 
  filter(word != '해') %>% 
  count(word,sScore,sort = T) %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(n,word,fill = sScore))+
  geom_col() + scale_fill_gradient() + 
  facet_wrap(~author,scales = 'free')
  
sosul2_df %>% unnest_tokens(word, main, token = tokenize_tidy) %>% 
  separate_wider_delim(word,delim = '/',names = c('word','type')) %>% 
  inner_join(senti_dic_df) %>% 
  group_by(author) %>% 
  summarise(n = sum(sScore))

test <- sosul2_df %>% unnest_tokens(word, main, token = tokenize_tidy) %>% 
  separate_wider_delim(word,delim = '/',names = c('word','type'))

# WordCloud ---------------------------------------------------------------

sosul2_df %>% unnest_tokens(word, main, token = tokenize_tidy) %>% 
  separate_wider_delim(word,delim = '/',names = c('word','type')) %>% 
  inner_join(senti_dic_df) %>% 
  filter(word != '해') %>% 
  mutate(emotion = case_when(sScore > 0 ~ '긍정',sScore < 0 ~ '부정', sScore == 0 ~ '중립')) %>% 
  filter(emotion != '중립' ) %>% 
  count(word, emotion ,sort =T) %>% 
  reshape2::acast(word ~ emotion,value.var = 'n',fill = 0) %>% 
  comparison.cloud(colors = c('skyblue','orange'),max.words = 50,title.size = 3)



# Change over time --------------------------------------------------------

sosul2_emo <- sosul2_df %>% 
  unnest_tokens(sentence, main, token = 'sentences') %>% 
  mutate(linenumber = row_number()) %>% 
  unnest_tokens(word, sentence,token = tokenize_tidy) %>% 
  separate_wider_delim(word,delim = '/',names = c('word','type')) %>% 
  inner_join(senti_dic_df) %>% 
  mutate(emotion = ifelse(sScore > 0, "긍정", 
                          ifelse(sScore < 0, "부정", "중립"))) %>% 
  count(author, index = linenumber %/% 10, emotion) %>% # 10개 행씩 묶기
  pivot_wider(names_from = emotion, values_from = n,values_fill = 0) %>% 
  mutate(sentiment = 긍정 - 부정)

sosul2_emo %>% ggplot(aes(x = index, y = sentiment, fill = author)) +
  geom_col() +
  facet_wrap( ~ author, scale = 'free') + geom_smooth()


