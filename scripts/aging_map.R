library(tidyverse)
library(rgdal)
library(raster)
library(rgeos)


sido_map <- readOGR("./data/sido_2021/bnd_sido_00_2021_2021_2Q.shp", encoding = "UTP-8")

mode(sido_map)
class(sido_map)

sido_map$BASE_DATE # meta 정보
sido_map@data # 실제 정보 접근, S4 속성의 특성임

## 메타 데이터
sido_map@data %>% dim()
sido_map@data %>% head()

## 실제 데이터
sido_map@polygons %>% length()
sido_map@polygons[1] # 첫 번째 시도 정보

length(sido_map)

## 시도 코드 데이테어 넣기
sido_info <- sido_map@data %>% 
  rownames_to_column(var = "id") %>% 
  as_tibble() %>% 
  rename_with(tolower)

sido_info %>% head()

# Map boundary dissolving
# plot(sido_map) # 굉장히 오래 걸림
plot(sido_map[sido_map$SIDO_NM == "서울특별시", ])
# plot(sido_map[sido_map$SIDO_NM == "부산광역시", ]) # 섬이 많아 굉장히 오래 걸림
plot(sido_map[sido_map$SIDO_NM == "대구광역시", ])
# plot(sido_map[sido_map$SIDO_NM == "전라남도", ]) # 섬이 많아 굉장히 오래 걸림

# 지도 해상도 조절하기
map_df <- rgeos::gSimplify(sido_map, 
                           tol = 100, 
                           topologyPreserve = TRUE)

plot(map_df)

# 사각형 데이터로 바꾸기 
map_df_tidy <- broom::tidy(map_df)
map_df_tidy %>% head()

sido_map@data

map_df_tidy %>% head()

map_df_tidy <- map_df_tidy %>% 
  filter(piece %in% c(1,2)) %>% 
  # mutate(group = fct_rev(group)) %>% 
  left_join(sido_info)

dim(map_df_tidy)
map_df_tidy %>% slice_head(n = 4)

map_df_tidy %>% 
  rename(지역 = sido_nm) %>% 
  ggplot(data = .) +
    geom_polygon(aes(x = long,
                     y = lat,
                     group = group,
                     fill = 지역), 
                 color = "black") +
    coord_sf(datum = sf::st_crs(5179)) +
    ggthemes::theme_map() + 
    theme(legend.position = "right")

## 노령화지수 데이터 불러오기 

age_df <- read.delim("./data/2021년기준_2020년_인구총괄(노령화지수).txt", header = FALSE, sep = "^") %>% 
  as_tibble()

age_df %>% head()

age_df <- age_df %>% 
  mutate(sido_cd = str_sub(V2, 1, 2), 
         age_score = V4,
         .keep = "none") %>% 
  group_by(sido_cd) %>% 
  summarize(age_score = mean(age_score))

age_df %>% slice_head(n = 4)
  

## 테이블 조인

map_df_tidy <- map_df_tidy %>% 
  left_join(age_df)
glimpse(map_df_tidy)

# 지도 시각화-노령화지수 연결
plot_df <- map_df_tidy %>% 
  rename(지역 = sido_nm,
          노령화지수 = age_score)
p <- ggplot(data = plot_df) +
  geom_polygon(
    aes(x = long, y = lat, 
        group = group,
        subgroup = 지역,
        fill = 노령화지수), 
    color = "black") +
  scale_fill_gradient(
    low = "blue", high = "red") + 
  coord_sf(datum = sf::st_crs(5179)) +
  ggthemes::theme_map(base_size = 12) +
  theme(legend.position = "right") + 
  labs(title = "전국시도별 노령화지수 시각화", 
       caption = "2021년 6월 30일 기준")

p

## 인터렉티브 시각화 
library(plotly)

ggplotly(p)  
