library(shiny)
library(tidyverse)


buzzes <- read_tsv("regionals18-tu.tsv") %>% 
  filter(!is.na(buzz_location)) %>% 
  mutate(answer = ifelse(answer %in% c("Australia", "California", "Finland"),
                         paste0(answer, " (", category, ")"),
                         answer),
         buzz_location_pct = factor(round(buzz_location_pct,2), levels = seq(0,1,.01)))
bonuses <- read_tsv("regionals18-b.tsv") %>% 
  unite(part1, answer1, value1, sep = " sanjuwefhnufweim ") %>% 
  unite(part2, answer2, value2, sep = " sanjuwefhnufweim ") %>% 
  unite(part3, answer3, value3, sep = " sanjuwefhnufweim ") %>% 
  gather(part, result, part1:part3) %>% 
  separate(result, c("answer", "value"), sep = " sanjuwefhnufweim ") %>% 
  mutate(packet_bonus_part = paste(packet, bonus, part, sep = ","),
         value = as.numeric(value)) %>% 
  filter(!is.na(team)) %>% 
  unique() %>% 
  group_by(packet_bonus_part) %>% 
  mutate(heard = n(),
         conv = sum(value/10)/n())
tossups <- buzzes %>% 
  select(answer, category, subcategory, packet) %>% 
  unique()

packets_heard <- buzzes %>% 
  mutate(tournament_room = paste(tournament, room, sep = " - ")) %>% 
  group_by(packet) %>% 
  summarize(heard = length(unique(tournament_room)))
packets_played <- buzzes %>% 
  group_by(team) %>% 
  summarize(games = length(unique(packet)))
packets_by_team <- buzzes %>% 
  count(team, packet) %>% 
  mutate(n = 1) %>% 
  spread(team, n, fill = 0)

buzzes <- left_join(buzzes, packets_heard)

all_pdf <- buzzes %>%
  filter(buzz_value == 10, is.na(bounceback)) %>% 
  group_by(answer, buzz_location_pct) %>% 
  mutate(buzzed = n()) %>% 
  select(answer, category, subcategory, heard, buzz_location_pct, buzzed) %>% 
  unique()

all_cdf <- read_csv("all_cdf.csv")

tossup_conversions <- all_cdf %>% 
  group_by(answer, category, subcategory) %>% 
  summarize(tu_conv = max(conv),
            heard = max(heard)) %>% 
  mutate(lab = paste0("Conversion: ", scales::percent(tu_conv), " (", heard, " rooms)"))

category_cdf <- all_cdf %>% 
  group_by(category, buzz_location_pct) %>% 
  summarize(conv = sum(all_buzzed)/sum(heard))

init_select <- sample(unique(buzzes$team), 2)

ui <- fluidPage(
  selectizeInput(
    "teams",
    "Teams",
    selected = c("Chicago A", "Penn A", "Berkeley A"),
    multiple = T,
    choices = buzzes %>%
      pull(tournament) %>%
      unique() %>% 
      map(~sort(unique(buzzes$team[buzzes$tournament == .]))) %>%
      setNames(unique(buzzes$tournament)),
    options = list(maxItems = 5)
  ),
  selectInput(
    "selected_cat",
    "Category",
    selected = "Literature",
    choices = buzzes %>%
      pull(category) %>%
      unique()
  ),
  actionButton("submit", "Submit"),
  plotOutput("mainPlot", height = "850px")
)

server <- shinyServer(function(input, output) {
  palette_maker <- reactive({
    palette <- c("dodgerblue", 
                 "maroon", 
                 "goldenrod",
                 "forestgreen",
                 "darkorchid")[1:length(input$teams)]
    palette <- c(palette, "gray") %>% 
      set_names(input$teams, "Other")
    return(palette)
  })
  
  packets_to_display <- reactive({
    packets_by_team %>% 
      select_at(vars(c("packet", input$teams))) %>% 
      filter_at(vars(-packet), all_vars(. == 1)) %>%
      pull(packet)
  })
  
  data_func <- eventReactive(input$submit, {
    dat <- buzzes %>% 
      filter(team %in% input$teams, 
             buzz_value == 10, 
             is.na(bounceback), 
             category == input$selected_cat,
             packet %in% packets_to_display()) %>% 
      select(answer, team, player, buzz_location_pct) %>% 
      separate(player, c("first_name", "last_name"), 
               sep = " ", remove = F, extra = "merge", fill = "left") %>% 
      mutate(initials = paste0(substr(first_name, 1, 1), 
                               substr(last_name, 1, 1)),
             buzz_location_pct = as.numeric(as.character(buzz_location_pct))) %>% 
      select(-first_name, -last_name) %>% 
      left_join(all_cdf, by = c("answer", "buzz_location_pct")) %>% 
      group_by(answer) %>% 
      mutate(buzz_rank = rank(buzz_location_pct, ties.method = "random"))
    return(dat)
  })
  
  plot_maker <- eventReactive(input$submit, {
    ggplot() +
      geom_area(data = filter(all_cdf, answer %in% data_func()$answer), 
                aes(x = buzz_location_pct, y = conv),
                fill = 'gray') +
      geom_col(data = data_func(), 
               aes(x = buzz_location_pct, y = conv+ifelse(conv < .05, .1, 0), fill = team), 
               width = .01) +
      geom_text(data = data_func(), 
                aes(x = buzz_location_pct, 
                    y = isolate(buzz_rank*-(.35/length(input$teams))),
                    label = initials, 
                    color = team), 
                size = 4.5, family = 'Lato') +
      geom_line(data = filter(category_cdf, category == input$selected_cat),
                aes(x = buzz_location_pct,
                    y = conv),
                linetype = 'dashed') +
      geom_text(data = filter(tossup_conversions, answer %in% data_func()$answer),
                aes(x = .25,
                    y = .85,
                    label = lab)) +
      facet_wrap(~answer) +
      scale_y_continuous(labels = scales::percent, 
                         limits = c(-0.38,1), 
                         breaks = seq(0,1,.25)) +
      scale_x_continuous(labels = scales::percent,
                         limits = c(0,1.01), 
                         breaks = seq(0,1,.2)) +
      labs(title = paste0("Top Teams on ", input$selected_cat),
           x = "% of Question Elapsed", 
           y = "% of Rooms with a correct buzz") +
      scale_fill_manual(values = palette_maker()) +
      scale_color_manual(values = palette_maker()) +
      guides(fill = guide_legend(title = "Team", title.hjust = 0.5), color = F) +
      theme_bw() +
      theme(text = element_text(size = 20, family = 'Lato'),
            panel.grid.minor = element_blank(),
            axis.text = element_text(size = 12),
            legend.key.height = unit(1.25, "cm"))
  })
  
  output$mainPlot <- renderPlot(plot_maker())
})

shinyApp(ui = ui, server = server)
