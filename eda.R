### Builing vs unit Condition ############
data %>% select(contains("Condition")) %>% 
  select(contains("None"),contains("Unable")) %>% 
  names() -> annoying
data %>% select(contains("Condition")) %>%
select(-one_of(annoying),`Condition Of Building`) %>%
rename(Building = `Condition Of Building`) %>%
gather(Condition, Response, contains("Condition")) %>% 
filter(!is.na(Response)) %>%
group_by(Condition, Building) %>%
summarise(Percent = sum(Response=="Yes")/n(),
          count = n()) %>%
ggplot(aes(Condition,Percent, fill=Building)) + 
  geom_col(position="dodge") + coord_flip() +
  scale_y_continuous(labels = percent) +
  facet_wrap(facets = ~Building)
data %>% select(contains("Condition"), Value, `Purchase Price 1`) %>%
  select(-one_of(annoying)) %>%
  rename(Building = `Condition Of Building`,
         Price = `Purchase Price 1`) %>%
  gather(Condition, Response, contains("Condition")) %>% 
  group_by(Condition, Response) %>%
  summarise(avgP = mean(Price,na.rm=T), count = n()) %>% 
  filter(!is.na(Response)) %>% 
  group_by(Condition) %>% 
  summarise(diff_avg_price = diff(-avgP),
                  no_count = count[1],
                 yes_count = count[2]) %>%
  mutate(plot_text = paste0("#No: ",no_count,"\n",
                            "#Yes: ",yes_count)) %>%
  arrange(diff_avg_price) %>% ungroup() %>%
  mutate(Condition = factor(Condition, levels = Condition)) %>%
  ggplot(aes(Condition, diff_avg_price)) + geom_col() + coord_flip()
### Price vs Value by building condition ##########
data %>% select(matches("Value|Price 1| Of Building")) %>% 
  rename(Price = `Purchase Price 1`, Building = `Condition Of Building`) %>%
  group_by(Building) %>% 
  ggplot(aes(Price,Value, col=Building)) + geom_point(position = "jitter")+
  geom_abline(slope = 1,intercept = 0) + facet_wrap(facets=~Building)
ggplot(data, aes(`Purchase Price 1`, Value, col = `Number Of Bedrooms`)) + 
  geom_point(position = "jitter", alpha = .5) +
  geom_abline(slope = 1,intercept = 0) + facet_wrap(facets = ~Borough)

### Borough ##########
ggplot(data,aes(`Purchase Price 1`)) + geom_histogram(binwidth = 5e+04) + 
  geom_density(aes(y=5e+04*..count..)) +
  facet_wrap(facets = ~Borough)
