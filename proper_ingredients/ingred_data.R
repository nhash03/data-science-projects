library(tidyverse)
library(dbplyr)
library(stringr)
library(tidytext)


ingred_data <- read_csv("test.csv", col_names  = FALSE, skip = 1) 
  
ingred_data <- ingred_data %>%
  select(-X1) %>%
  pivot_longer(cols = X2:X51, names_to = "name", values_to = "ingreds") %>%
  filter(!is.na(ingreds)) %>%
  select(ingreds)
no_rep_ingred <- ingred_data %>% distinct()

ingred_data2 <- read_csv("csvjson.csv")
ingred_data2 <- ingred_data2 %>% select(ingredients) %>%
  unnest_tokens(word, ingredients) %>%
  rename(ingreds = word)

ingred_tot <- rbind(no_rep_ingred, ingred_data2) %>% distinct()
ingred_vec <- as_vector(ingred_tot)

data <- read_csv("recipes_82k.csv")
data <- data %>%
  mutate (data, ID = 1:nrow(data))


data <- mutate(data,
               new_ingredients = 
                 gsub("\\s*\\([^\\)]+\\)","",
                      as.character(data$ingredients)))

data <- mutate(data,
               new_ingredients = 
                 gsub("([0-9]*\\s)?([0-9]+(/[0-9]+)?\\s[a-z]*\\s)","",
                      as.character(data$new_ingredients))) %>%
  select(ID, new_ingredients)



  
#data_select <- data %>%
#  select(ID, new_ingredients) %>%
#  mutate(test_ID = floor(ID / 5000))
remove_table <- read_csv("common_words1.csv")
removes <- remove_table %>%
  select(word) %>%
  slice(1:22, 24, 26:38, 41:42, 44, 46:49, 51:53, 55:58, 60:63,
        65, 66,69:71,73,77,80:88, 91, 92, 94, 97, 100, 101, 105:107,
        109, 111, 115, 116, 118, 120, 125, 127, 130, 133, 134, 137, 138, 
        142:150, 152:153, 155, 157, 159, 163:165, 167:170, 172:175,
        184, 187:192, 195:201,203, 207, 210, 216:221, 223, 226, 228:230,
        232, 234, 236, 238:244, 246, 249, 251:253, 259, 260, 264:266, 276,
        279:280, 282:283, 288, 293,295, 297:300, 303:305) %>%
  as_vector()

clear_file <- read_csv("ingredients.csv", col_names = FALSE)
clear_file <- clear_file %>%
  select(X2) %>%
  unnest_tokens(word, X2) %>%
  as_vector()

df_ingrd <- data %>%
  unnest_tokens(word, new_ingredients) %>%
  filter(word %in% ingred_vec) %>%
  filter(!(word %in% removes)) %>%
  filter(word %in% clear_file) %>%
  group_by(ID) %>%
  count(word) %>%
  spread(key = word, value = n, fill = 0) %>%
  ungroup()

df_ingrd <- df_ingrd %>%
  mutate_at(vars(a:zucchini), function(x) ifelse (x > 0, 1, 0))


ingreds <- colnames(df_ingrd)

my_ingredients <- readline(prompt = "What do I have in my fridge?: ")
my_ingredients <- my_ingredients %>%
  strsplit(split = ", ") %>%
  as_vector()

my_ingredients_copy <- my_ingredients[my_ingredients %in% ingreds] 

df_ingrd$tot_num = rowSums(df_ingrd[,c(-1)])

can_recipe <- select(df_ingrd, my_ingredients_copy, ID, tot_num) 
num_of_ingredients <- length(my_ingredients_copy)
can_recipe$ingreds_num = rowSums(can_recipe[,c(-(num_of_ingredients+1), 
                                             -(num_of_ingredients+2))]) 
can_recipe <- can_recipe %>%
  filter(ingreds_num > 0) %>%
  mutate(diff =  tot_num - num_of_ingredients) %>%
  arrange(diff) %>%
  select(ID, diff)

original_data <- read_csv("recipes_82k.csv") 
original_data <- mutate (original_data, ID = 1:nrow(data)) %>%
  
final_recipes <- left_join(can_recipe, original_data) %>%
  select(recipe_name, ingredients) %>%
  slice(1:5)

























