

```{r}


avgClassSize <- minnesota_xc_clean %>%
  filter(team %in% miacSchools) %>%
  #get rid of duplication of 2014 conference results listed as 2013
  filter(meet_name != "MIAC Cross Country Championships") %>% 
  mutate(class_year = substr(class_year,1,2)) %>% 
  filter(class_year %in% c("FR","SO","JR","SR")) %>% 
  group_by(class_year, race_group,meet_year) %>% 
  select(name,class_year,race_group,meet_year) %>% 
  distinct() %>% 
  summarise(classSize = n())

avgClassSize %>% 
  filter(race_group=="Women") %>% 
  ggplot(aes(meet_year,classSize,group=class_year,color=class_year)) +
  geom_line(size=1) +
  geom_point() +
  labs(title="Number of Women Racing in the MIAC by Class Year") +
  xlab("Year") +
  ylab("Numer of Athletes") +
  guides(color=guide_legend(title="Class Year"))

```

```{r}
avgClassSize %>% 
  filter(race_group=="Men") %>% 
  ggplot(aes(meet_year,classSize,group=class_year,color=class_year)) +
  geom_line(size=1) +
  geom_point() +
  labs(title="Number of Men Racing in the MIAC by Class Year") +
  xlab("Year") +
  ylab("Numer of Athletes") +
  guides(color=guide_legend(title="Class Year"))
```


```{r}
avgTimesByClass <- minnesota_xc_clean %>% 
  filter(grepl("MIAC|Minnesota Intercollegiate Championships", meet_name)) %>%
  mutate(class_year = substr(class_year,1,2)) %>% 
  filter(class_year %in% c("FR","SO","JR","SR")) %>% 
  group_by(meet_year,class_year,meet_kilometers,race_group) %>% 
  summarise(averageTime = mean(new_time),
            athleteCount = n()) 

avgTimesByClass %>% 
  filter(race_group=="Women") %>% 
  ggplot(aes(meet_year,averageTime,color=class_year,group=class_year)) +
  geom_point(size=2) +
  geom_line() +
  labs(title="Women MIAC Championships Average Time by Class Year") + 
  xlab("Year") +
  ylab("Average Time") +
  guides(color=guide_legend(title="Class Year"))
```

```{r}
avgTimesByClass %>% 
  filter(race_group=="Men") %>% 
  ggplot(aes(meet_year,averageTime,color=class_year,group=class_year)) +
  geom_point(size=2) +
  geom_line() +
  labs(title="Men MIAC Championships Average Time by Class Year") + 
  xlab("Year") +
  ylab("Average Time") +
  guides(color=guide_legend(title="Class Year"))
```

###Find 2013 Results

```{r}
miacChamps2013 <- read_csv("C:/Users/CPC24/Documents/HeatherRTrainingFun/miacChamps2013.csv")


miacChamps2013Cleaning <- miacChamps2013 %>% 
  filter(row_number()!=63) %>% 
  mutate(race_group = ifelse((row_number()<245),"Women","Men"),
         place_score_time = gsub("[a-zA-Z,]*","",data),
         name_year_school = gsub("[0-9:\\.]","",data),
         year_school = gsub("(.*,){1}","",name_year_school),
         name = gsub("(, .*){1}","",name_year_school),
         meet_date="2013-11-02",
         meet_name="MIAC Championships",
         meet_location="Como Park Golf Course",
         time = str_extract(place_score_time, "\\d{2}:\\d{2}.*"),
         time = substr(time,1,7),
         place_score = gsub("\\d{2}:\\d{2}.*","",place_score_time),
         start_time="",
         url="",
         meet_year="2013",
         meet_kilometers = ifelse((race_group=="Woman"),6,8)) %>% 
  separate(time, c("race_minutes", "race_seconds"), sep = ":", remove = FALSE) %>% 
  mutate(new_time = as.numeric(race_minutes) + (as.numeric(race_seconds)/60)) %>% 
  select(-name_year_school) %>% 
  separate(name,c("empty","first","last")) %>% 
  select(-empty) %>% 
  unite("name",c("last","first"),sep=", ",remove=TRUE) %>% 
  separate(year_school,c("empty","class_year","school1","school2")) %>% 
  replace_na(list(school2="")) %>% 
  select(-empty) %>% 
  unite("team",c("school1","school2"),sep=" ",remove=TRUE) %>% 
  separate(place_score,c("place","score")) %>%
  mutate(avg_time = ifelse((race_group=="Women"),new_time/3.72823,new_time/4.97097)) %>% 
  separate(avg_time,c("avgMin","avgSecs"),remove=FALSE) %>% 
  mutate(avgSecs = round(as.numeric(avgSecs)*60/100000000000000,0),
         secs = ifelse((avgSecs < 10),paste(0,avgSecs,sep=""),paste(avgSecs)),
         avg_mile = paste(avgMin,":",secs,sep="")) %>% 
  select(meet_name,meet_date,meet_location,start_time,race_minutes,race_seconds,race_group,place,name,class_year,team,avg_mile,time,score,url,meet_year,meet_kilometers,new_time)

colnames(minnesota_xc_clean)

write.csv(miacChamps2013Cleaning,"C:/Users/CPC24/Documents/HeatherRTrainingFun/miacChamps2013CLEAN.csv")

miacResultsClean <- rbind(minnesota_xc_clean %>% select(-X),miacChamps2013Cleaning)


write.csv(miacResultsClean,"C:/Users/CPC24/Documents/HeatherRTrainingFun/miacCCResultsAllClean171031.csv")


```