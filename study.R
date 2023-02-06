#https://bookdown.org/ahn_media/bookdown-demo/cleantool.html#purrr
# Set Enviro --------------------------------------------------------------
rm(list=ls())
library(tidyverse)
library(conflicted)
library(elbird)
library(tidytext)
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
library(modelsummary)

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
library(epubr)

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
library(rvest)
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
