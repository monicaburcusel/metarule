#Research questions: 
  #1.How do meta-rules impact performance? (between levels, game types and models)
  #2.How do meta-rules impact specific rules? (between levels, game types and models)

#Note: Baseline and Ground rules are used interchangeably 

----#1.Setup and data preparation (a-f)----
  #1a. Load libraries 
  library(readxl)
  library(tidyverse)
  library(broom)
  library(emmeans)
  library(DHARMa)
  library(performance)
  library(car)
  library(ggrepel)
  library(stringr)

  #1b. Load excel file
  df <- read_excel("internship_results_final.xlsx")

  #1c. Ground-rule data frame (all)
  df <- df %>%
    mutate(num_incorrect = num_total - num_correct)

  #1d. Define datasets for different sections
  #Section 2: ALL models (including Llama and Gemini)
  df_ground <- df  

  #Section 3: Exclude Llama and Gemini, keep humans and O3
  df_meta <- df %>%
    filter(!(model %in% c("gemini_2.0", "llama_4_maverick")))

  # Section 4: Exclude Llama, Gemini, and humans (only AI models)
  df_stats <- df %>%
    filter(!(model %in% c("gemini_2.0", "llama_4_maverick", "human")))

  #1e. Color schemes for different sections
  #Section 2: ALL models (8 models total)
  all_models_colors <- c(
    "Human" = "wheat3",
    "Claude 4" = "#FBB4AE",
    "Deepseek V3" = "#B3CDE3",
    "GPT 4o" = "#CCEBC5",
    "Grok 3 Beta" = "#FFFFCC",
    "O3" = "plum2",
    "Llama 4" = "mistyrose",
    "Gemini 2" = "#FED9A6"
  )

  #Section 3: Exclude Llama and Gemini (6 models total)
  meta_analysis_colors <- c(
    "Human" = "wheat3",
    "Claude 4" = "#FBB4AE",
    "Deepseek V3" = "#B3CDE3",
    "GPT 4o" = "#CCEBC5",
    "Grok 3 Beta" = "#FFFFCC",
    "O3" = "plum2"
  )

  #Section 4: Only AI models (5 models total)
  ai_only_colors <- c(
    "Claude 4" = "#FBB4AE",
    "Deepseek V3" = "#B3CDE3",
    "GPT 4o" = "#CCEBC5",
    "Grok 3 Beta" = "#FFFFCC",
    "O3" = "plum2"
  )

  #1f. Game type colors (same for all sections)
  game_colors <- c(
    "counting" = "cyan3",
    "non_counting" = "chocolate2"
  )

  #1g. Meta rule level colors (same for all sections)
  meta_rule_colors <- c(
    "0" = "powderblue",
    "1" = "yellow3",
    "2" = "salmon2",
    "Baseline" = "powderblue",
    "MR1" = "yellow3",
    "MR2" = "salmon2"
  )

  #1h. Function for consistent model names
  standardize_all_model_names <- function(df) {
    df %>%
      mutate(
        model_display = case_when(
          model == "human" ~ "Human",
          model == "claude_4" ~ "Claude 4",
          model == "deepseek_v3" ~ "Deepseek V3",
          model == "gpt_4o" ~ "GPT 4o",
          model == "grok_3_beta" ~ "Grok 3 Beta",
          model == "o3" ~ "O3",
          model == "llama_4_maverick" ~ "Llama 4",
          model == "gemini_2.0" ~ "Gemini 2",
          TRUE ~ tools::toTitleCase(str_replace_all(model, "_", " "))
        )
      )
  }
  
  standardize_meta_model_names <- function(df) {
    df %>%
      mutate(
        model_display = case_when(
          model == "human" ~ "Human",
          model == "claude_4" ~ "Claude 4",
          model == "deepseek_v3" ~ "Deepseek V3",
          model == "gpt_4o" ~ "GPT 4o",
          model == "grok_3_beta" ~ "Grok 3 Beta",
          model == "o3" ~ "O3",
          TRUE ~ tools::toTitleCase(str_replace_all(model, "_", " "))
        )
      )
  }
  
  standardize_ai_model_names <- function(df) {
    df %>%
      mutate(
        model_display = case_when(
          model == "claude_4" ~ "Claude 4",
          model == "deepseek_v3" ~ "Deepseek V3",
          model == "gpt_4o" ~ "GPT 4o",
          model == "grok_3_beta" ~ "Grok 3 Beta",
          model == "o3" ~ "O3",
          TRUE ~ tools::toTitleCase(str_replace_all(model, "_", " "))
        )
      )
  }

----#2. Ground rule analysis (all models including LLAMA and GEMINI) (a-d)----
  
  #2a. Specific ground rules: Models' performance on specific ground rules
  #Creating ground rule df (all models)
  ground_df <- df_ground %>%
  filter(meta_rule_level == 0) %>%
  filter(prompt == "prompt_1") %>%
  mutate(accuracy = round((num_correct / num_total) * 100, 1))

  #Game 1: Letter-string analogies (counting)
  ground_counting <- ground_df %>% 
    filter(game_type == "counting")

  #Game 1: Ground rule df with prompt 1 only
  ground_counting_avg <- ground_counting %>%
    standardize_all_model_names() %>%
    group_by(model_display, game_type, ground_rule) %>%
    summarise(
      num_correct = sum(num_correct),
      num_total = sum(num_total),
      accuracy = round((num_correct / num_total) * 100, 1),
      .groups = "drop"
    ) %>%
    mutate(
      model_display = factor(model_display, levels = names(all_models_colors))
    )
  
  #Game 1: Plot of performance on game 1
  ggplot(ground_counting_avg, aes(x = ground_rule, y = accuracy, fill = model_display)) +
    geom_col(position = position_dodge2(width = 0.9, padding = 0.1)) +
    geom_text(
      aes(label = accuracy), 
      position = position_dodge2(width = 0.9, padding = 0.1), 
      vjust = -0.25,
      size = 2.5,
      color = "black"
    ) +
    scale_fill_manual(values = all_models_colors) + 
    scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = "Letter String Analogies: Models' Performance on Ground Rules",
      y = "Accuracy (%)",
      x = "Ground Rule",
      fill = "Model"
    ) +
    theme_gray(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 0, vjust = 0.5)
    )

  #Game 2: Four-term analogies (Non_counting)
  ground_non_counting <- ground_df %>% 
    filter(game_type == "non_counting")
  
  #Game 2: Df with ground rules and non_counting game
  ground_non_counting_avg <- ground_non_counting %>%
    standardize_all_model_names() %>%
    group_by(model_display, game_type, ground_rule) %>%
    summarise(
      num_correct = sum(num_correct),
      num_total = sum(num_total),
      accuracy = round((num_correct / num_total) * 100, 1),
      .groups = "drop"
    ) %>%
    mutate(
      model_display = factor(model_display, levels = names(all_models_colors))
    )
  
  #Game 2: Plot ground rules and non_counting game
  ggplot(ground_non_counting_avg, aes(x = ground_rule, y = accuracy, fill = model_display)) +
    geom_col(position = position_dodge2(width = 0.9, padding = 0.1)) +
    geom_text(
      aes(label = accuracy),
      position = position_dodge2(width = 0.9, padding = 0.1),
      vjust = -0.25,
      size = 2.5,
      color = "black"
    ) +
    scale_fill_manual(values = all_models_colors) +
    scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = "Four Term Analogies: Models' Performance on Ground Rules",
      y = "Accuracy (%)",
      x = "Ground Rule",
      fill = "Model"
    ) +
    theme_gray(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 0, vjust = 0.5)
    )

  #2b. Ground rules collapsed: How do games compare? 
  plot_overall_ground_accuracy <- function() {
    ground_accuracy_by_game <- ground_df %>%
      standardize_all_model_names() %>%
      group_by(model_display, game_type) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round((total_correct / total_trials) * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model_display = factor(model_display, levels = names(all_models_colors))
      )
    
    ggplot(ground_accuracy_by_game, aes(x = model_display, y = accuracy, fill = game_type)) +
      geom_col(position = position_dodge(width = 0.8)) +
      geom_text(
        aes(label = accuracy),
        position = position_dodge(width = 0.8),
        vjust = -0.5,
        size = 2.5,
        color = "black"
      ) +
      scale_fill_manual(values = game_colors) +
      scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
      labs(
        title = "Performance on ground rules by game type",
        y = "Accuracy (%)",
        x = "Model",
        fill = "Game type"
      ) +
      theme_gray(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.title = element_text(face = "bold")
      )
  }
  
  #2b. Performance by games
  plot_overall_ground_accuracy()

  #2c. Game type collapsed: How did models perform overall on ground rules
  plot_overall_model_accuracy <- function() {
    model_accuracy <- ground_df %>%
      standardize_all_model_names() %>%
      group_by(model_display) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round((total_correct / total_trials) * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model_display = factor(model_display, levels = names(all_models_colors))
      )
    
    ggplot(model_accuracy, aes(x = model_display, y = accuracy, fill = model_display)) +
      geom_col() +
      geom_text(
        aes(label = accuracy),
        vjust = -0.5,
        size = 3,
        color = "black"
      ) +
      scale_fill_manual(values = all_models_colors) +
      scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
      labs(
        title = "Overall Performance on Ground Rules",
        x = "Model",
        y = "Accuracy (%)"
      ) +
      theme_gray(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.position = "none"
      )
  }
  
  #2c. Overall model accuracy
  plot_overall_model_accuracy()

----#3. Meta-rule analysis (a-c)---- (excluding LLAMA and GEMINI)
  #3a. For each specific rule (ground rule, meta 1 and meta 2)
  plot_rule_meta_bar <- function(game_type_filter) {
    rule_bar_summary <- df_meta %>%
      filter(game_type == game_type_filter) %>%
      filter(prompt == "prompt_1") %>%
      standardize_meta_model_names() %>%
      group_by(model_display, game_type, ground_rule, meta_rule_level) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round((total_correct / total_trials) * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model_display = factor(model_display, levels = names(meta_analysis_colors)),
        ground_rule = tools::toTitleCase(ground_rule)
      )
    
    game_name <- if (game_type_filter %in% c("counting")) {
      "Letter-string analogies"
    } else if (game_type_filter %in% c("fourterm", "non_counting")) {
      "Four-term analogies"
    } else {
      game_type_filter
    }
    
    full_title <- paste0(game_name, ": Effect of meta-rules on specific analogies")
    
    ggplot(rule_bar_summary, aes(x = factor(meta_rule_level), y = accuracy, fill = model_display)) +
      geom_col(position = position_dodge(width = 0.8)) +
      geom_text(
        aes(label = accuracy),
        position = position_dodge(width = 0.8),
        vjust = -0.3,
        size = 2,
        color = "black"
      ) +
      facet_wrap(~ ground_rule) +
      scale_y_continuous(limits = c(0, 105), expand = expansion(mult = c(0, 0.03))) +
      scale_fill_manual(values = meta_analysis_colors) +
      labs(
        title = full_title,
        x = "Meta-rule level (0 = Ground rule, 1 = MR1, 2 = MR2)",
        y = "Accuracy (%)",
        fill = "Model"
      ) +
      theme_gray(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text.x = element_text(angle = 0, vjust = 1),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        strip.text = element_text(size = 9, face = "bold"),
        legend.title = element_text(face = "bold")
      )
  }

  #3a BAR. Game 1 and 2
  plot_rule_meta_bar("counting")
  plot_rule_meta_bar("non_counting")

  #Version 2of 3a: (After Dr. Pinto's feedback): Function for single analogy and game type
  plot_single_analogy_meta_bar <- function(game_type_filter, analogy_name) {
    rule_bar_summary <- df_meta %>%
      filter(game_type == game_type_filter) %>%
      filter(prompt == "prompt_1") %>%
      standardize_meta_model_names() %>%
      filter(ground_rule == analogy_name) %>%
      group_by(model_display, meta_rule_level) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round((total_correct / total_trials) * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model_display = factor(model_display, levels = names(meta_analysis_colors)),
        ground_rule = tools::toTitleCase(analogy_name)
      )
    
    game_name <- if (game_type_filter %in% c("counting")) {
      "Letter-string analogies"
    } else if (game_type_filter %in% c("fourterm", "non_counting")) {
      "Four-term analogies"
    } else {
      game_type_filter
    }
    
    ggplot(rule_bar_summary, aes(x = factor(meta_rule_level), y = accuracy, fill = model_display)) +
      geom_col(position = position_dodge(width = 0.8), color = "black") +
      geom_text(
        aes(label = accuracy),
        position = position_dodge(width = 0.8),
        vjust = -0.5,
        size = 5,
        fontface = "bold",
        color = "black"
      ) +
      scale_y_continuous(limits = c(0, 105), expand = expansion(mult = c(0, 0.04))) +
      scale_fill_manual(values = meta_analysis_colors) +
      labs(
        x = "Meta-rule level (0 = Ground rule, 1 = MR1, 2 = MR2)",
        y = "Accuracy (%)",
        fill = "Participant"
      ) +
      theme_classic(base_size = 18) +
      theme(
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        plot.title = element_blank()
      )
  }

  #Looping over all letter-string analogies
  unique_counting_analogies <- unique(df_meta$ground_rule[df_meta$game_type == "counting"])
  
  for (analogy in unique_counting_analogies) {
    print(plot_single_analogy_meta_bar("counting", analogy))
  }

  #Looping overall all four-term analogies
  unique_noncounting_analogies <- unique(df_meta$ground_rule[df_meta$game_type == "non_counting"])
  
  for (analogy in unique_noncounting_analogies) {
    print(plot_single_analogy_meta_bar("non_counting", analogy))
  }

  #3a. Line plot
  plot_rule_meta_line <- function(game_type_filter) {
    rule_line_summary <- df_meta %>%
      filter(game_type == game_type_filter) %>%
      filter(prompt == "prompt_1") %>%
      standardize_meta_model_names() %>%
      group_by(model_display, game_type, ground_rule, meta_rule_level) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round((total_correct / total_trials) * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model_display = factor(model_display, levels = names(meta_analysis_colors)),
        ground_rule = tools::toTitleCase(ground_rule)
      )
    
    game_name <- case_when(
      game_type_filter == "counting" ~ "Letter String Analogies",
      game_type_filter %in% c("non_counting", "fourterm") ~ "Four Term Analogies",
      TRUE ~ game_type_filter
    )
    
    ggplot(rule_line_summary, aes(x = factor(meta_rule_level), y = accuracy, group = model_display, color = model_display)) +
      geom_line(size = 1.2) +
      geom_point(size = 2.0) +
      geom_text_repel(aes(label = accuracy), 
                      size = 2, 
                      color = "black",
                      box.padding = 0.1,
                      point.padding = 0.1,
                      min.segment.length = 0,
                      max.overlaps = Inf,
                      force = 2,
                      seed = 42) +
      facet_wrap(~ ground_rule) +
      scale_color_manual(values = meta_analysis_colors) +
      labs(
        title = paste0(game_name, ": Effect of Meta-rules on Specific Rules"),
        x = "Meta-rule level (0 = Ground rule, 1 = MR1, 2 = MR2)",
        y = "Accuracy (%)",
        color = "Model"
      ) +
      theme_gray(base_size = 12) +
      theme(
        strip.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.margin = margin(5, 5, 5, 5),
        panel.spacing = unit(0.1, "lines")
      ) +
      scale_x_discrete(expand = c(0.05, 0.01)) +
      scale_y_continuous(limits = c(0, 105), expand = c(0.01, 0))
  }

  # 3a line: Game 1 and 2
  plot_rule_meta_line("counting")
  plot_rule_meta_line("non_counting")

  #3b. Rule type collapsed
  plot_meta_bar <- function() {
    meta_summary <- df_meta %>%
      filter(prompt == "prompt_1") %>%
      filter(game_type %in% c("counting", "non_counting")) %>%
      standardize_meta_model_names() %>%
      group_by(model_display, meta_rule_level, game_type) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round((total_correct / total_trials) * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model_display = factor(model_display, levels = names(meta_analysis_colors)),
        game_type = case_when(
          game_type == "counting" ~ "Letter-string analogies",
          game_type == "non_counting" ~ "Four-term analogies",
          TRUE ~ game_type
        )
      )
    
    ggplot(meta_summary, aes(x = factor(meta_rule_level), y = accuracy, fill = model_display)) +
      geom_col(position = position_dodge(width = 0.9)) +
      geom_text(
        aes(label = accuracy),
        position = position_dodge(width = 0.9),
        vjust = -0.5,
        size = 2.5,
        color = "black"
      ) +
      facet_wrap(~ game_type) +
      scale_y_continuous(limits = c(0, 107), expand = expansion(mult = c(0, 0.02))) +
      scale_fill_manual(values = meta_analysis_colors) +
      labs(
        title = "Effect of meta-rules on games",
        x = "Meta-rule level (0 = Ground rule, 1 = MR1, 2 = MR2)",
        y = "Accuracy (%)",
        fill = "Model"
      ) +
      theme_gray(base_size = 12) +
      theme(
        strip.text = element_text(size = 9, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  }

  #Plotting bar chart
  plot_meta_bar()

  #3b Version 2 After Dr. Pinto's feedback - Letter-string
  plot_meta_bar_letter_string <- function() {
    meta_summary <- df_meta %>%
      filter(prompt == "prompt_1") %>%
      filter(game_type == "counting") %>%
      standardize_meta_model_names() %>%
      group_by(model_display, meta_rule_level) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round((total_correct / total_trials) * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model_display = factor(model_display, levels = names(meta_analysis_colors))
      )
    
    ggplot(meta_summary, aes(x = factor(meta_rule_level), y = accuracy, fill = model_display)) +
      geom_col(position = position_dodge(width = 0.9), color = "black") +
      geom_text(
        aes(label = accuracy),
        position = position_dodge(width = 0.9),
        vjust = -0.5,
        size = 5,
        fontface = "bold",
        color = "black"
      ) +
      scale_y_continuous(limits = c(0, 107), expand = expansion(mult = c(0, 0.04))) +
      scale_fill_manual(values = meta_analysis_colors) +
      labs(
        x = "Meta-rule level (0 = Ground rule, 1 = MR1, 2 = MR2)",
        y = "Accuracy (%)",
        fill = "Participant"
      ) +
      theme_classic(base_size = 18) +
      theme(
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        plot.title = element_blank()
      )
  }

  #Plotting version 2 of 3b
  plot_meta_bar_letter_string()

  #Version 2 of 3b after Dr. Pinto's feedback - four-term analogies 
  plot_meta_bar_four_term <- function() {
    meta_summary <- df_meta %>%
      filter(prompt == "prompt_1") %>%
      filter(game_type == "non_counting") %>%
      standardize_meta_model_names() %>%
      group_by(model_display, meta_rule_level) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round((total_correct / total_trials) * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model_display = factor(model_display, levels = names(meta_analysis_colors))
      )
    
    ggplot(meta_summary, aes(x = factor(meta_rule_level), y = accuracy, fill = model_display)) +
      geom_col(position = position_dodge(width = 0.9), color = "black") +
      geom_text(
        aes(label = accuracy),
        position = position_dodge(width = 0.9),
        vjust = -0.5,
        size = 5,
        fontface = "bold",
        color = "black"
      ) +
      scale_y_continuous(limits = c(0, 107), expand = expansion(mult = c(0, 0.04))) +
      scale_fill_manual(values = meta_analysis_colors) +
      labs(
        x = "Meta-rule level (0 = Ground rule, 1 = MR1, 2 = MR2)",
        y = "Accuracy (%)",
        fill = "Participant"
      ) +
      theme_classic(base_size = 18) +
      theme(
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        plot.title = element_blank()
      )
  }

  #Plotting version 2 of 3b four-term analogies 
  plot_meta_bar_four_term()

  #3b. Line
  plot_meta_line <- function() {
    performance_summary <- df_meta %>%
      filter(game_type %in% c("counting", "non_counting")) %>%
      filter(prompt == "prompt_1") %>%
      standardize_meta_model_names() %>%
      group_by(game_type, meta_rule_level, model_display) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round((total_correct / total_trials) * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model_display = factor(model_display, levels = names(meta_analysis_colors)),
        game_type = case_when(
          game_type == "counting" ~ "Letter String Analogies",
          game_type == "non_counting" ~ "Four Term Analogies",
          TRUE ~ game_type
        )
      )
    
    ggplot(performance_summary, aes(x = factor(meta_rule_level), y = accuracy, group = model_display, color = model_display)) +
      geom_line(size = 1.2) +
      geom_point(size = 2.5) +
      geom_text_repel(aes(label = accuracy), 
                      size = 2.5, 
                      color = "black",
                      box.padding = 0.1,
                      point.padding = 0.1,
                      min.segment.length = 0,
                      max.overlaps = Inf,
                      force = 2,
                      seed = 42) +
      facet_wrap(~ game_type) +
      scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.02))) +
      scale_color_manual(values = meta_analysis_colors) +
      labs(
        title = "Accuracy by meta-rule level",
        x = "Meta-rule level (0 = Ground rule, 1 = MR1, 2 = MR2)",
        y = "Accuracy (%)",
        color = "Model"
      ) +
      theme_gray(base_size = 12) +
      theme(
        strip.text = element_text(size = 9, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  }

  #3b. Line results
  plot_meta_line()

  #3c. Collapse by gametype
  plot_overall_meta_bar <- function() {
    df_filtered <- df_meta %>%
      filter(game_type %in% c("counting", "non_counting")) %>%
      filter(prompt == "prompt_1")
    
    summary <- df_filtered %>%
      standardize_meta_model_names() %>%
      group_by(model_display, meta_rule_level) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round(total_correct / total_trials * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model_display = factor(model_display, levels = names(meta_analysis_colors))
      )
    
    ggplot(summary, aes(x = model_display, y = accuracy, fill = factor(meta_rule_level))) +
      geom_col(position = position_dodge(width = 0.9)) +
      geom_text(
        aes(label = accuracy),
        position = position_dodge(width = 0.9),
        vjust = -0.3,
        size = 2.5,
        color = "black",
        show.legend = FALSE
      ) +
      scale_y_continuous(limits = c(0, 107), expand = expansion(mult = c(0, 0.02))) +
      scale_fill_manual(
        values = c("0" = "powderblue", "1" = "yellow3", "2" = "salmon2"),
        name = "Meta-rule level",
        labels = c("Ground rules", "MR1", "MR2")
      ) +
      labs(
        title = "Overall impact of meta-rules",
        x = "Model",
        y = "Accuracy (%)"
      ) +
      theme_gray(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.title = element_text(face = "bold")
      )
  }

  #3c. Bar plot
  plot_overall_meta_bar()

  #Version 2 of 3c After Dr. Pinto's feedback: Bar for reasoning models and general-purpose
  #Reasoning models + Human
  plot_overall_meta_bar_reasoning <- function() {
    summary <- df_meta %>%
      standardize_meta_model_names() %>%
      filter(model_display %in% c("Human", "Claude 4", "Grok 3 Beta", "O3")) %>%
      group_by(model_display, meta_rule_level) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round(total_correct / total_trials * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model_display = factor(model_display, levels = c("Human", "Claude 4", "Grok 3 Beta", "O3")),
        meta_rule_level = as.character(meta_rule_level)
      )
    
    ggplot(summary, aes(x = model_display, y = accuracy, fill = meta_rule_level)) +
      geom_col(position = position_dodge(width = 0.9), color = "black") +
      geom_text(
        aes(label = accuracy),
        position = position_dodge(width = 0.9),
        vjust = -0.5,
        size = 5,
        fontface = "bold",
        color = "black"
      ) +
      scale_y_continuous(limits = c(0, 107), expand = expansion(mult = c(0, 0.02))) +
      scale_fill_manual(
        values = c("0" = "powderblue", "1" = "yellow3", "2" = "salmon2"),
        name = "Meta-rule level",
        labels = c("Ground rule", "MR1", "MR2")
      ) +
      labs(
        x = "Participant",
        y = "Accuracy (%)",
        fill = "Meta-rule level"
      ) +
      theme_classic(base_size = 18) +
      theme(
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        plot.title = element_blank()
      )
  }
  plot_overall_meta_bar_reasoning()

  #General purpose
  plot_overall_meta_bar_general <- function() {
    summary <- df_meta %>%
      standardize_meta_model_names() %>%
      filter(model_display %in% c("Deepseek V3", "GPT 4o")) %>%
      group_by(model_display, meta_rule_level) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round(total_correct / total_trials * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model_display = factor(model_display, levels = c("Deepseek V3", "GPT 4o")),
        meta_rule_level = as.character(meta_rule_level) # Ensures color mapping works
      )
    
    ggplot(summary, aes(x = model_display, y = accuracy, fill = meta_rule_level)) +
      geom_col(position = position_dodge(width = 0.9), color = "black") +
      geom_text(
        aes(label = accuracy),
        position = position_dodge(width = 0.9),
        vjust = -0.5,
        size = 5,
        fontface = "bold",
        color = "black"
      ) +
      scale_y_continuous(limits = c(0, 107), expand = expansion(mult = c(0, 0.02))) +
      scale_fill_manual(
        values = c("0" = "powderblue", "1" = "yellow3", "2" = "salmon2"),
        name = "Meta-rule level",
        labels = c("Ground rule", "MR1", "MR2")
      ) +
      labs(
        x = "Participant",
        y = "Accuracy (%)",
        fill = "Meta-rule level"
      ) +
      theme_classic(base_size = 18) +
      theme(
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        plot.title = element_blank()
      )
  }
  
  #The plot
  plot_overall_meta_bar_general()

  #3c. Line plot function
  plot_overall_meta_line <- function() {
    df_filtered <- df_meta %>%
      filter(game_type %in% c("counting", "non_counting")) %>%
      filter(prompt == "prompt_1")
    
    summary <- df_filtered %>%
      standardize_meta_model_names() %>%
      group_by(model_display, meta_rule_level) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round(total_correct / total_trials * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model_display = factor(model_display, levels = names(meta_analysis_colors))
      )
    
    ggplot(summary, aes(x = factor(meta_rule_level), y = accuracy, group = model_display, color = model_display)) +
      geom_line(size = 1.2) +
      geom_point(size = 2.5) +
      geom_text_repel(
        aes(label = accuracy),
        size = 2.5,
        color = "black",
        box.padding = 0.1,
        point.padding = 0.1,
        min.segment.length = 0,
        max.overlaps = Inf,
        force = 2,
        seed = 42
      ) +
      scale_color_manual(values = meta_analysis_colors) +
      scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.02))) +
      labs(
        title = "Overall impact of meta-rules",
        x = "Meta-rule level (0 = Ground rule, 1 = MR1, 2 = MR2)",
        y = "Accuracy (%)",
        color = "Model"
      ) +
      theme_gray(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.title = element_text(face = "bold")
      )
  }
  
  #Line plots
  plot_overall_meta_line()

----# 4. Statistical Analysis (a-e)-----
  #4a. Data preparation 
  df_stats_main <- df_stats %>%
    filter(game_type %in% c("counting", "non_counting")) %>%
    filter(meta_rule_level %in% c(0, 1)) %>%  
    filter(prompt == "prompt_1") %>%  #Only prompt 1
    mutate(num_incorrect = num_total - num_correct)

  cat(sprintf("Data after filtering: %d observations\n", nrow(df_stats_main)))
  cat("Models in filtered data:", unique(df_stats_main$model), "\n")

  #Checking that calculations match raw data
  verification <- df_stats_main %>%
    group_by(meta_rule_level) %>%
    summarise(
      total_correct = sum(num_correct),
      total_trials = sum(num_total),
      accuracy = round((total_correct / total_trials) * 100, 1),
      n_obs = n(),
      .groups = "drop")
  print(verification)

  #Checking the data
  cat("Missing values:", sum(is.na(df_stats_main)), "\n")
  cat("Zero trials:", sum(df_stats_main$num_total == 0, na.rm = TRUE), "\n")
  cat("Impossible values (correct > total):", sum(df_stats_main$num_correct > df_stats_main$num_total, na.rm = TRUE), "\n")

  #Check for potential separation issues
  separation_check <- df_stats_main %>%
    mutate(accuracy = num_correct / num_total) %>%
    group_by(model, game_type, meta_rule_level) %>%
    summarise(
      min_acc = min(accuracy, na.rm = TRUE),
      max_acc = max(accuracy, na.rm = TRUE),
      n_obs = n(),
      .groups = "drop"
    ) %>%
    filter(min_acc == 0 | max_acc == 1 | n_obs < 2)
  
  if(nrow(separation_check) > 0) {
    cat("WARNING: Potential separation issues in these conditions:\n")
    print(separation_check)
  } else {
    cat("✓ No separation issues detected\n")
  }

  #Formatting the labels
  df_stats_main <- df_stats_main %>%
    mutate(
      meta_rule_level = factor(meta_rule_level, levels = c(0, 1), labels = c("Baseline", "MR1")),
      model_clean = case_when(
        model == "claude_4" ~ "Claude 4",
        model == "deepseek_v3" ~ "Deepseek V3", 
        model == "gpt_4o" ~ "GPT 4o",
        model == "grok_3_beta" ~ "Grok 3 Beta",
        model == "o3" ~ "O3",
        TRUE ~ as.character(model)
      ),
      game_type_clean = case_when(
        game_type == "counting" ~ "Letter String Analogies",
        game_type == "non_counting" ~ "Four Term Analogies",
        TRUE ~ as.character(game_type)
      ),
      ground_rule = factor(tools::toTitleCase(ground_rule))
    ) %>%
    mutate(
      model = factor(model_clean),
      game_type = factor(game_type_clean)
    ) %>%
    select(-model_clean, -game_type_clean)
  
    cat("Models:", levels(df_stats_main$model), "\n")
    cat("Games:", levels(df_stats_main$game_type), "\n")
    cat("Meta levels:", levels(df_stats_main$meta_rule_level), "\n")

  #Colors for plots
  ai_only_colors <- c("Claude 4" = "#FBB4AE", "Deepseek V3" = "#B3CDE3", 
                      "GPT 4o" = "#CCEBC5", "Grok 3 Beta" = "#FFFFCC", "O3" = "plum2")
  game_colors <- c("Letter String Analogies" = "cyan3", "Four Term Analogies" = "chocolate2")
  meta_rule_colors <- c("Baseline" = "powderblue", "MR1" = "yellow3")

  #4b. Selecting the right statistical model
  fit_model_safely <- function(formula, data, name) {
    cat(sprintf("Fitting %s...\n", name))
    
    tryCatch({
      #Trying quasibinomial first (handles overdispersion)
      model <- glm(formula, data = data, family = quasibinomial,
                   control = glm.control(maxit = 100, epsilon = 1e-8))
      
      if(model$converged) {
        cat(sprintf("✓ %s converged (quasibinomial)\n", name))
        return(list(model = model, family = "quasibinomial"))
      } else {
        #Trying regular binomial if quasibinomial fails
        cat("Trying binomial family...\n")
        model <- glm(formula, data = data, family = binomial,
                     control = glm.control(maxit = 100, epsilon = 1e-8))
        
        if(model$converged) {
          cat(sprintf("✓ %s converged (binomial)\n", name))
          return(list(model = model, family = "binomial"))
        } else {
          cat(sprintf("✗ %s failed to converge\n", name))
          return(NULL)
        }
      }
    }, error = function(e) {
      cat(sprintf("✗ %s failed with error: %s\n", name, e$message))
      return(NULL)
    })
  }

  #Fitting models from simple to complex
  simple_result <- fit_model_safely(
    cbind(num_correct, num_incorrect) ~ meta_rule_level + model + game_type,
    df_stats_main, "Simple model (main effects only)"
  )
  
  medium_result <- fit_model_safely(
    cbind(num_correct, num_incorrect) ~ meta_rule_level * model + meta_rule_level * game_type + model * game_type,
    df_stats_main, "Medium model (all 2-way interactions)"
  )
  
  complex_result <- fit_model_safely(
    cbind(num_correct, num_incorrect) ~ meta_rule_level * model * game_type,
    df_stats_main, "Complex model (3-way interaction)"
  )

  #Extracting models
  simple_model <- if(!is.null(simple_result)) simple_result$model else NULL
  medium_model <- if(!is.null(medium_result)) medium_result$model else NULL
  complex_model <- if(!is.null(complex_result)) complex_result$model else NULL

  #Checking available models
  available_models <- list()
  if(!is.null(simple_model)) available_models$simple <- simple_model
  if(!is.null(medium_model)) available_models$medium <- medium_model
  if(!is.null(complex_model)) available_models$complex <- complex_model
  
  if(length(available_models) == 0) {
    cat("ERROR: No models converged. Using descriptive analysis instead.\n")
    use_descriptive <- TRUE
  } else {
    use_descriptive <- FALSE
  }
  
  #Model diagnostics (comparing all three models)
  model_diagnostics <- data.frame(
    Model = character(),
    Deviance = numeric(),
    AIC = numeric(),
    Parameters = numeric(),
    Obs_per_Param = numeric(),
    Dispersion = numeric(),
    Pseudo_R2 = numeric(),
    stringsAsFactors = FALSE
  )
  
  for(i in 1:length(available_models)) {
    model_name <- names(available_models)[i]
    model_obj <- available_models[[i]]
    
    dev <- model_obj$deviance
    aic_val <- tryCatch(AIC(model_obj), error = function(e) NA)
    params <- length(coef(model_obj))
    obs_per_param <- nrow(df_stats_main) / params
    dispersion <- ifelse(model_obj$family$family == "quasibinomial", 
                         summary(model_obj)$dispersion, 
                         dev / model_obj$df.residual)
    pseudo_r2 <- 1 - (dev / model_obj$null.deviance)
    
    model_diagnostics <- rbind(model_diagnostics, data.frame(
      Model = model_name,
      Deviance = round(dev, 1),
      AIC = ifelse(is.na(aic_val), NA, round(aic_val, 1)),
      Parameters = params,
      Obs_per_Param = round(obs_per_param, 1),
      Dispersion = round(dispersion, 3),
      Pseudo_R2 = round(pseudo_r2, 3)
    ))
  }
  print(model_diagnostics)
  
  #Selecting the right model (lower deviance better)
  for(i in 1:length(deviance_values)) {
    cat(sprintf("%s: %.1f deviance\n", names(deviance_values)[i], deviance_values[i]))
  }

  #Selecting the best model (prefering parsimony)
  best_model_name <- names(which.min(deviance_values))
  primary_model <- available_models[[best_model_name]]
  
  cat(sprintf("\nInitial selection: %s model (lowest deviance: %.1f)\n", best_model_name, min(deviance_values)))
  
  #Test if complex model is significantly better than medium
  if(length(available_models) > 1 && !is.null(medium_model) && !is.null(complex_model)) {
    cat("\nTesting if complex model significantly improves fit:\n")
    tryCatch({
      anova_test <- anova(medium_model, complex_model, test = "F")
      print(anova_test)
      
      complex_p <- anova_test$`Pr(>F)`[2]
      deviance_reduction <- medium_model$deviance - complex_model$deviance
      
      cat(sprintf("Complex vs Medium: Deviance reduction = %.1f, p = %.4f\n", 
                  deviance_reduction, complex_p))
      
      if(complex_p > 0.05) {
        cat("→ Complex model not significantly better, using medium model for parsimony\n")
        primary_model <- medium_model
        best_model_name <- "medium"
      } else {
        cat("→ Complex model significantly better, justified complexity\n")
      }
    }, error = function(e) {
      cat("Model comparison failed, using deviance criterion\n")
    })
  }
  
  cat(sprintf("\nFINAL MODEL SELECTION: %s model\n", best_model_name))
  
  #Assessing the ginal model
  final_disp <- ifelse(primary_model$family$family == "quasibinomial", 
                       summary(primary_model)$dispersion, 
                       primary_model$deviance / primary_model$df.residual)
  final_pseudo_r2 <- 1 - (primary_model$deviance / primary_model$null.deviance)
  final_obs_per_param <- nrow(df_stats_main) / length(coef(primary_model))
  
  cat(sprintf("Final model diagnostics:\n"))
  cat(sprintf("  Dispersion: %.3f %s\n", final_disp, 
              ifelse(final_disp < 2, "(good)", ifelse(final_disp < 4, "(acceptable)", "(high - concerning)"))))
  cat(sprintf("  Pseudo R²: %.1f%% of variance explained\n", final_pseudo_r2 * 100))
  cat(sprintf("  Sample adequacy: %.1f observations per parameter %s\n", 
              final_obs_per_param, ifelse(final_obs_per_param > 10, "(excellent)", ifelse(final_obs_per_param > 5, "(adequate)", "(limited)"))))
  
  if(final_disp > 3) {
    cat("⚠ WARNING: High overdispersion detected - interpret p-values cautiously\n")
  } else {
    cat("✓ Model diagnostics acceptable for reliable inference\n")
  }

  #4c. RQ1: Impact of meta-rule 1 on performance
  #Calculating the effect size
  rq1_means_corrected <- df_stats_main %>%
    group_by(meta_rule_level) %>%
    summarise(
      total_correct = sum(num_correct),
      total_trials = sum(num_total),
      accuracy = round((total_correct / total_trials) * 100, 1),
      se = sqrt((accuracy/100) * (1 - accuracy/100) / total_trials) * 100,
      ci_lower = accuracy - 1.96 * se,
      ci_upper = accuracy + 1.96 * se,
      .groups = "drop"
    )
  
  performance_drop_corrected <- rq1_means_corrected$accuracy[1] - rq1_means_corrected$accuracy[2]
  
  cat(sprintf("Baseline accuracy: %.1f%% (95%% CI: %.1f%% - %.1f%%) [%d correct / %d trials]\n", 
              rq1_means_corrected$accuracy[1], 
              rq1_means_corrected$ci_lower[1],
              rq1_means_corrected$ci_upper[1],
              rq1_means_corrected$total_correct[1],
              rq1_means_corrected$total_trials[1]))
  cat(sprintf("MR1 accuracy: %.1f%% (95%% CI: %.1f%% - %.1f%%) [%d correct / %d trials]\n", 
              rq1_means_corrected$accuracy[2],
              rq1_means_corrected$ci_lower[2],
              rq1_means_corrected$ci_upper[2],
              rq1_means_corrected$total_correct[2], 
              rq1_means_corrected$total_trials[2]))
  cat(sprintf("CORRECT Performance drop: %.1f percentage points\n", performance_drop_corrected))
  
  #Effect size interpretation with corrected values
  if(performance_drop_corrected > 30) {
    cat("*** VERY LARGE effect size (>30 percentage points)\n")
  } else if(performance_drop_corrected > 15) {
    cat("** LARGE effect size (15-30 percentage points)\n")
  } else if(performance_drop_corrected > 5) {
    cat("* MEDIUM effect size (5-15 percentage points)\n")
  } else {
    cat("SMALL effect size (<5 percentage points)\n")
  }
  
  #Cohen's d for more precise effect size
  baseline_var <- rq1_means_corrected$accuracy[1] * (100 - rq1_means_corrected$accuracy[1]) / 100
  mr1_var <- rq1_means_corrected$accuracy[2] * (100 - rq1_means_corrected$accuracy[2]) / 100
  pooled_sd <- sqrt((baseline_var + mr1_var) / 2)
  cohens_d <- performance_drop_corrected / pooled_sd
  
  cat(sprintf("Cohen's d equivalent: %.2f\n", cohens_d))
  if(cohens_d > 1.2) {
    cat("Very large standardised effect\n")
  } else if(cohens_d > 0.8) {
    cat("Large standardised effect\n")
  } else if(cohens_d > 0.5) {
    cat("Medium standardised effect\n")
  } else {
    cat("Small standardised effect\n")
  }
  
  #Version 2 of Cohen's D #Correct Cohen's D
  #Calculating Cohen's D
  #Rq1: Meta-rule level impact on performance values 
  mean_baseline <- 92.1
  mean_mr1 <- 63.5
  se_baseline <- 0.190
  se_mr1 <- 0.761
  n_baseline <- 20000
  n_mr1 <- 4000
  
  cohen_d_from_means <- function(mean1, mean2, se1, se2, n1, n2) {
    # Convert SE to SD
    sd1 <- se1 * sqrt(n1)
    sd2 <- se2 * sqrt(n2)
    
    # Calculate pooled SD
    pooled_sd <- sqrt( ((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2) )
    
    # Calculate Cohen's d
    d <- (mean1 - mean2) / pooled_sd
    
    # Return the value
    return(round(d, 3))
  }
  
  # Run the function
  cohen_d <- cohen_d_from_means(mean_baseline, mean_mr1, se_baseline, se_mr1, n_baseline, n_mr1)
  cat("Cohen's d:", cohen_d)
  
  #Full ANOVA analysis
  if(!use_descriptive) {
    tryCatch({
      anova_results <- Anova(primary_model, type = "II", test = "F")
      
      cat("ANOVA Results (Type II):\n")
      print(anova_results)
      
      #Extract and interpret all effects
      if("meta_rule_level" %in% rownames(anova_results)) {
        meta_rule_p <- anova_results["meta_rule_level", "Pr(>F)"]
        meta_rule_f <- anova_results["meta_rule_level", "F value"]
        
        cat("\nCORRECTED STATISTICAL RESULTS (AI Models Only):\n")
        cat(sprintf("Meta-rule main effect: F = %.2f, p = %.4f\n", meta_rule_f, meta_rule_p))
        
        if(meta_rule_p < 0.001) {
          cat("*** HIGHLY SIGNIFICANT: Meta-rules significantly hurt AI performance ***\n")
        } else if(meta_rule_p < 0.05) {
          cat("** SIGNIFICANT: Meta-rules significantly hurt AI performance **\n")
        } else {
          cat("* NOT SIGNIFICANT: No clear meta-rule effect on AI models *\n")
        }
        
        #Calculate effect sizes
        eta_squared <- anova_results["meta_rule_level", "Sum Sq"] / sum(anova_results[!grepl("Residuals", rownames(anova_results)), "Sum Sq"])
        cat(sprintf("Eta-squared (effect size): %.3f %s\n", eta_squared,
                    ifelse(eta_squared > 0.14, "(large)", ifelse(eta_squared > 0.06, "(medium)", "(small)"))))
      }
      
      #Model effects
      if("model" %in% rownames(anova_results)) {
        model_p <- anova_results["model", "Pr(>F)"]
        model_f <- anova_results["model", "F value"]
        cat(sprintf("Model main effect: F = %.2f, p = %.4f\n", model_f, model_p))
        
        if(model_p < 0.05) {
          cat("** SIGNIFICANT: AI Models show different overall performance **\n")
        } else {
          cat("* NOT SIGNIFICANT: AI Models perform similarly overall *\n")
        }
      }
      
      #Game effects
      if("game_type" %in% rownames(anova_results)) {
        game_p <- anova_results["game_type", "Pr(>F)"]
        game_f <- anova_results["game_type", "F value"]
        cat(sprintf("Game type main effect: F = %.2f, p = %.4f\n", game_f, game_p))
        
        if(game_p < 0.05) {
          cat("** SIGNIFICANT: Game types differ in difficulty **\n")
        } else {
          cat("* NOT SIGNIFICANT: Game types equally difficult *\n")
        }
      }
      
      #Interaction effects
      cat("\nINTERACTION ANALYSIS:\n")
      
      if("meta_rule_level:model" %in% rownames(anova_results)) {
        model_meta_p <- anova_results["meta_rule_level:model", "Pr(>F)"]
        model_meta_f <- anova_results["meta_rule_level:model", "F value"]
        cat(sprintf("AI Model × Meta-rule interaction: F = %.2f, p = %.4f\n", model_meta_f, model_meta_p))
        
        if(model_meta_p < 0.05) {
          cat("** SIGNIFICANT: Meta-rule 1 affects AI models differently **\n")
        } else {
          cat("* NOT SIGNIFICANT: Meta-rule 1 affects AI models the same *\n")
        }
      }
      
      if("meta_rule_level:game_type" %in% rownames(anova_results)) {
        game_meta_p <- anova_results["meta_rule_level:game_type", "Pr(>F)"]
        game_meta_f <- anova_results["meta_rule_level:game_type", "F value"]
        cat(sprintf("Game × Meta-rule interaction: F = %.2f, p = %.4f\n", game_meta_f, game_meta_p))
        
        if(game_meta_p < 0.05) {
          cat("** SIGNIFICANT: Meta-rule 1 affects game types differently **\n")
        } else {
          cat("* NOT SIGNIFICANT: Meta-rule 1 affects game types the same *\n")
        }
      }
      
      if("model:game_type" %in% rownames(anova_results)) {
        model_game_p <- anova_results["model:game_type", "Pr(>F)"]
        model_game_f <- anova_results["model:game_type", "F value"]
        cat(sprintf("Model × Game interaction: F = %.2f, p = %.4f\n", model_game_f, model_game_p))
        
        if(model_game_p < 0.05) {
          cat("** SIGNIFICANT: Models are affected differently across games **\n")
        } else {
          cat("* NOT SIGNIFICANT: Models are affected the same across games *\n")
        }
      }
      
      #Three-way interaction
      if("meta_rule_level:model:game_type" %in% rownames(anova_results)) {
        three_way_p <- anova_results["meta_rule_level:model:game_type", "Pr(>F)"]
        three_way_f <- anova_results["meta_rule_level:model:game_type", "F value"]
        cat(sprintf("Three-way Model × Game × Meta-rule interaction: F = %.2f, p = %.4f\n", three_way_f, three_way_p))
        
        if(three_way_p < 0.05) {
          cat("** SIGNIFICANT: Complex interaction **\n")
        } else {
          cat("* NOT SIGNIFICANT: No three-way interaction *\n")
        }
      }
      
    }, error = function(e) {
      cat("ANOVA failed with error:", e$message, "\n")
      cat("Proceeding with descriptive analysis...\n")
      use_descriptive <<- TRUE
    })
  }
  
  #Fit a GLM with a three way interaction 
    primary_model_three_way <- glm(
    cbind(num_correct, num_incorrect) ~ model * game_type * meta_rule_level,
    data = df_stats_main,
    family = quasibinomial)
  
  #Type II ANOVA to test for main effects and interactions
  anova_three_way <- Anova(primary_model_three_way, type = "II", test = "F")
  
  #Print results
  print(anova_three_way)
  
  #Extract three-way interactions
  if ("model:game_type:meta_rule_level" %in% rownames(anova_three_way)) {
    f_val <- anova_three_way["model:game_type:meta_rule_level", "F value"]
    p_val <- anova_three_way["model:game_type:meta_rule_level", "Pr(>F)"]
    
    cat(sprintf("\nThree-way interaction (Model × Game × Meta-rule): F = %.2f, p = %.4f\n", f_val, p_val))
    
    if (p_val < 0.05) {
      cat("** SIGNIFICANT: Meta-rule 1 effects depend on specific model-game combinations **\n")
    } else {
      cat("* NOT SIGNIFICANT: Meta-rule 1 effects are consistent across model-game combinations *\n")
    }
  } else {
    cat("\n Three-way interaction term not found in the model output. Check model formula or data.\n")
  }

  #Descriptive analysis
  #1. Overall effect of meta-rule 1 with confidence intervals
  print(rq1_means_corrected)

  #2. Where certain models affected by meta-rule 1 more than others?
  rq1_by_model_corrected <- df_stats_main %>%
    group_by(model, meta_rule_level) %>%
    summarise(
      total_correct = sum(num_correct),
      total_trials = sum(num_total),
      accuracy = round((total_correct / total_trials) * 100, 1),
      se = sqrt((accuracy/100) * (1 - accuracy/100) / total_trials) * 100,
      ci_lower = accuracy - 1.96 * se,
      ci_upper = accuracy + 1.96 * se,
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = meta_rule_level, values_from = c(accuracy, total_correct, total_trials, se, ci_lower, ci_upper)) %>%
    mutate(
      drop = accuracy_Baseline - accuracy_MR1,
      drop_se = sqrt(se_Baseline^2 + se_MR1^2),
      drop_ci_lower = drop - 1.96 * drop_se,
      drop_ci_upper = drop + 1.96 * drop_se,
      vulnerability_rank = rank(-drop),
      percent_reduction = round((drop / accuracy_Baseline) * 100, 1)
    ) %>%
    arrange(desc(drop))
    print(rq1_by_model_corrected %>% 
            select(model, accuracy_Baseline, accuracy_MR1, drop, drop_ci_lower, drop_ci_upper, percent_reduction, vulnerability_rank))

  #From most to least vulnerable models
  most_vulnerable_model <- rq1_by_model_corrected$model[1]
  least_vulnerable_model <- tail(rq1_by_model_corrected$model, 1)
  max_drop <- max(rq1_by_model_corrected$drop)
  min_drop <- min(rq1_by_model_corrected$drop)

  cat(sprintf("\nMost vulnerable AI model: %s (%.1f%% drop, %.1f%% relative reduction)\n", 
              most_vulnerable_model, max_drop, rq1_by_model_corrected$percent_reduction[1]))
  cat(sprintf("Least vulnerable AI model: %s (%.1f%% drop, %.1f%% relative reduction)\n", 
              least_vulnerable_model, min_drop, tail(rq1_by_model_corrected$percent_reduction, 1)))
  cat(sprintf("Vulnerability range: %.1f%% to %.1f%% (%.1f%% spread)\n", 
              min_drop, max_drop, max_drop - min_drop))

  #3. Effect of meta-rule 1 on game types
  rq1_by_game_corrected <- df_stats_main %>%
    group_by(game_type, meta_rule_level) %>%
    summarise(
      total_correct = sum(num_correct),
      total_trials = sum(num_total),
      accuracy = round((total_correct / total_trials) * 100, 1),
      se = sqrt((accuracy/100) * (1 - accuracy/100) / total_trials) * 100,
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = meta_rule_level, values_from = c(accuracy, total_correct, total_trials, se)) %>%
    mutate(
      drop = accuracy_Baseline - accuracy_MR1,
      percent_reduction = round((drop / accuracy_Baseline) * 100, 1)
    )
  print(rq1_by_game_corrected)

  #4. Model x Game type
  model_game_breakdown <- df_stats_main %>%
    group_by(model, game_type, meta_rule_level) %>%
    summarise(
      accuracy = round(sum(num_correct) / sum(num_total) * 100, 1),
      n_trials = sum(num_total),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = meta_rule_level, values_from = c(accuracy, n_trials)) %>%
    mutate(drop = accuracy_Baseline - accuracy_MR1) %>%
    arrange(desc(drop))
  
  cat("\nMost to least affected):\n")
  print(model_game_breakdown)

  #5. Statistical power analysis
  total_observations <- nrow(df_stats_main)
  observations_per_condition <- total_observations / (2 * length(unique(df_stats_main$model)) * length(unique(df_stats_main$game_type)))
  
  cat(sprintf("Total observations: %d\n", total_observations))
  cat(sprintf("Average observations per condition: %.1f\n", observations_per_condition))

  if(observations_per_condition < 5) {
    cat("Low power for complex interactions (recommend >10 obs per condition)\n")
  } else if(observations_per_condition < 10) {
    cat("Moderate power for main effects, limited for interactions\n")
  } else {
    cat("Adequate power for reliable inference\n")
  }

  #Visualization 1: Overall impact with confidence intervals
  p1_corrected <- ggplot(rq1_means_corrected, aes(x = meta_rule_level, y = accuracy, fill = meta_rule_level)) +
    geom_col(alpha = 0.8, width = 0.6) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, size = 1) +
    geom_text(aes(label = paste0(round(accuracy, 1), "%")), vjust = -0.5, size = 5, fontface = "bold") +
    scale_fill_manual(values = meta_rule_colors) +
    scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
    labs(x = "Meta-rule level", y = "Accuracy (%)") +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          legend.position = "none",
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 13, face = "bold"))
  
  print(p1_corrected)

  #Visualization 1 After Dr. Pinto's comments
    #1. Convert to character to avoid factor issues
    rq1_means_corrected$meta_rule_level <- as.character(rq1_means_corrected$meta_rule_level)
    
    #2. Remove extra spaces just in case
    rq1_means_corrected$meta_rule_level <- trimws(rq1_means_corrected$meta_rule_level)
    
    #3. Replace "Baseline" with "Ground rule"
    rq1_means_corrected$meta_rule_level[rq1_means_corrected$meta_rule_level == "Baseline"] <- "Ground rule"
    
    #4. Make it a factor with desired order
    rq1_means_corrected$meta_rule_level <- factor(
      rq1_means_corrected$meta_rule_level,
      levels = c("Ground rule", "MR1")
    )
  
    #5. Check to confirm
    print(unique(rq1_means_corrected$meta_rule_level))
    
    #6. Colors
    meta_rule_colors <- c("Ground rule" = "powderblue", "MR1" = "yellow3")
  
    #7. Plot
    p1_corrected <- ggplot(rq1_means_corrected, aes(x = meta_rule_level, y = accuracy, fill = meta_rule_level)) +
      geom_col(alpha = 0.8, width = 0.6, color = "black") +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, size = 1) +
      geom_text(
        aes(label = paste0(round(accuracy, 1), "%")),
        vjust = -0.8, size = 6, fontface = "bold"
      ) +
      scale_fill_manual(values = meta_rule_colors) +
      scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
      labs(
        x = "Meta-rule level",
        y = "Accuracy (%)"
      ) +
      theme_minimal(base_size = 18) +
      theme(
        plot.title = element_blank(),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 16, face = "bold"),
        legend.position = "none"
      )
    
    print(p1_corrected)

  #Visualization 2 After Dr. Pinto's feedback
  p2_corrected <- ggplot(rq1_by_model_corrected, aes(x = reorder(model, -drop), y = drop, fill = model)) +
    geom_col(alpha = 0.8, color = "black") +
    geom_errorbar(aes(ymin = drop_ci_lower, ymax = drop_ci_upper), width = 0.3, size = 1) +
    geom_text(
      aes(y = drop_ci_upper + 2.5, label = paste0(round(drop, 1), "%")), 
      size = 5, fontface = "bold"
    ) +
    scale_fill_manual(values = ai_only_colors) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
    labs(
      x = "Model",
      y = "Accuracy decline (%)"
    ) +
    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_blank(), 
      axis.title.x = element_text(size = 20, face = "bold"),
      axis.title.y = element_text(size = 20, face = "bold"),
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 16, face = "bold"),
      axis.text.y = element_text(size = 16, face = "bold"),
      legend.position = "none"
    )
  
  print(p2_corrected)

  #Visualization 3: Line plot (not used)
  model_trajectories <- df_stats_main %>%
    group_by(model, meta_rule_level) %>%
    summarise(
      accuracy = sum(num_correct) / sum(num_total) * 100,
      se = sqrt((accuracy/100) * (1 - accuracy/100) / sum(num_total)) * 100,
      .groups = "drop"
    )
  
  model_trajectories <- model_trajectories %>%
    mutate(nudge_y = case_when(
      model == "Claude 4" & meta_rule_level == "Baseline" ~ 1,
      model == "Claude 4" & meta_rule_level == "MR1" ~ 2,
      model == "GPT 4o" & meta_rule_level == "Baseline" ~ -1,
      model == "GPT 4o" & meta_rule_level == "MR1" ~ 1,
      model == "Deepseek V3" & meta_rule_level == "Baseline" ~ 1,
      model == "Deepseek V3" & meta_rule_level == "MR1" ~ -2,
      model == "Grok 3 Beta" & meta_rule_level == "Baseline" ~ -2,
      model == "Grok 3 Beta" & meta_rule_level == "MR1" ~ -5,
      model == "O3" & meta_rule_level == "Baseline" ~ 3,
      model == "O3" & meta_rule_level == "MR1" ~ 2,
      TRUE ~ 0
    ))
  
  #Plotting
  p3_trajectories <- ggplot(model_trajectories, aes(x = meta_rule_level, y = accuracy, color = model, group = model)) +
    geom_line(size = 1.5, alpha = 0.8) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = accuracy - 1.96 * se, ymax = accuracy + 1.96 * se), 
                  width = 0.1, size = 1) +
    geom_text_repel(aes(label = round(accuracy, 1), y = accuracy + nudge_y),
                    size = 2.5, fontface = "bold", color = "black",
                    max.overlaps = Inf, segment.color = "grey50") +
    scale_color_manual(values = ai_only_colors) +
    scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.1))) +
    labs(title = "Impact of meta-rules on models",
         x = "Meta rule level", y = "Accuracy (%)", color = "Model") +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          legend.position = "bottom",
          legend.title = element_text(face = "bold"),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 13, face = "bold"))
  print(p3_trajectories)
  
  #Version 2 of visualiation 3 (used)
  p3_trajectories <- ggplot(model_trajectories, aes(x = meta_rule_level, y = accuracy, color = model, group = model)) +
    geom_line(size = 1.5, alpha = 0.8) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = accuracy - 1.96 * se, ymax = accuracy + 1.96 * se), 
                  width = 0.1, size = 1) +
    geom_text_repel(
      aes(label = round(accuracy, 1)),
      size = 2.5, color = "black",
      segment.color = "black",    
      segment.size = 0.5,
      max.overlaps = Inf,
      min.segment.length = 0,     
      box.padding = 0.15,       
      point.padding = 0.25
    ) +
    scale_color_manual(values = ai_only_colors) +
    scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.1))) +
    labs(title = "Impact of meta-rules on models",
         x = "Meta rule level", y = "Accuracy (%)", color = "Model") +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          legend.position = "bottom",
          legend.title = element_text(face = "bold"),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 13, face = "bold"))
  print(p3_trajectories)


  #4d. RQ2: Effect of meta-rule 1 on specific analogies (rules/transformations)
  #Preparing data
  df_stats_rules <- df_stats %>%
    filter(game_type %in% c("counting", "non_counting")) %>%
    filter(prompt == "prompt_1") %>%
    mutate(
      meta_rule_level = factor(meta_rule_level, levels = c(0, 1, 2), 
                               labels = c("Baseline", "MR1", "MR2")),
      model_clean = case_when(
        model == "claude_4" ~ "Claude 4",
        model == "deepseek_v3" ~ "Deepseek V3", 
        model == "gpt_4o" ~ "GPT 4o",
        model == "grok_3_beta" ~ "Grok 3 Beta",
        model == "o3" ~ "O3",
        TRUE ~ as.character(model)
      ),
      game_type_clean = case_when(
        game_type == "counting" ~ "Letter String Analogies",
        game_type == "non_counting" ~ "Four Term Analogies",
        TRUE ~ as.character(game_type)
      ),
      ground_rule = factor(tools::toTitleCase(ground_rule))
    ) %>%
    mutate(
      model = factor(model_clean),
      game_type = factor(game_type_clean)
    ) %>%
    select(-model_clean, -game_type_clean)
  
  cat(sprintf("RQ2 data: %d observations across %d ground rules\n", 
              nrow(df_stats_rules), length(unique(df_stats_rules$ground_rule))))

  #Focusing on ground rules vs. MR1 for statistical power
  df_stats_rules_main <- df_stats_rules %>%
    filter(meta_rule_level %in% c("Baseline", "MR1"))


  #Testing the stats. model for RQ2
  rq2_model <- glm(cbind(num_correct, num_incorrect) ~ 
                     meta_rule_level * ground_rule + model + game_type,
                   data = df_stats_rules_main, family = quasibinomial)

  #RQ statistical analysis: 
  if(rq2_model$converged) {
    cat("RQ2 model converged successfully\n")
    rq2_anova <- Anova(rq2_model, type = "II", test = "F")
    cat("\nRQ2 ANOVA Results:\n")
    print(rq2_anova)
    #Key results
    if("meta_rule_level:ground_rule" %in% rownames(rq2_anova)) {
      rule_interaction_p <- rq2_anova["meta_rule_level:ground_rule", "Pr(>F)"]
      rule_interaction_f <- rq2_anova["meta_rule_level:ground_rule", "F value"]
      
      cat("\nEffect of meta-rule 1 on analogies (AI Models Only):\n")
      cat(sprintf("Meta-rule × Ground rule interaction: F = %.2f, p = %.4f\n", 
                  rule_interaction_f, rule_interaction_p))
      
      if(rule_interaction_p < 0.05) {
        cat("** SIGNIFICANT: Meta-rule 1 affects analogies differently**\n")
      } else {
        cat("* NOT SIGNIFICANT: Meta-rule 1 affects analogies the same *\n")
      }
      
      #Effects size for analogies 
      rule_eta_squared <- rq2_anova["meta_rule_level:ground_rule", "Sum Sq"] / 
        sum(rq2_anova[!grepl("Residuals", rownames(rq2_anova)), "Sum Sq"])
      cat(sprintf("Rule interaction effect size (eta²): %.3f %s\n", rule_eta_squared,
                  ifelse(rule_eta_squared > 0.14, "(large)", ifelse(rule_eta_squared > 0.06, "(medium)", "(small)"))))
    }
    
    #Ground rule main effect
    if("ground_rule" %in% rownames(rq2_anova)) {
      rule_main_p <- rq2_anova["ground_rule", "Pr(>F)"]
      rule_main_f <- rq2_anova["ground_rule", "F value"]
      cat(sprintf("Ground rule main effect: F = %.2f, p = %.4f\n", rule_main_f, rule_main_p))
      
      if(rule_main_p < 0.05) {
        cat("** SIGNIFICANT: Analogies differ in overall difficulty **\n")
      } else {
        cat("* NOT SIGNIFICANT: Analogies equally difficult overall *\n")
      }
    }
    
  } else {
    cat("RQ2 model failed to converge, using descriptive analysis only\n")
    rule_interaction_p <- NA
  }

  #Detailed RQ2 statistical analysis
  #1. Impact of meta-rule 1 on specific analogies (Ground rule to MR1)
  rule_vulnerability <- df_stats_rules_main %>%
    group_by(ground_rule, meta_rule_level) %>%
    summarise(
      total_correct = sum(num_correct),
      total_trials = sum(num_total),
      accuracy = round((total_correct / total_trials) * 100, 1),
      se = sqrt((accuracy/100) * (1 - accuracy/100) / total_trials) * 100,
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = meta_rule_level, values_from = c(accuracy, total_correct, total_trials, se)) %>%
    mutate(
      vulnerability = accuracy_Baseline - accuracy_MR1,
      vulnerability_se = sqrt(se_Baseline^2 + se_MR1^2),
      vulnerability_ci_lower = vulnerability - 1.96 * vulnerability_se,
      vulnerability_ci_upper = vulnerability + 1.96 * vulnerability_se,
      vulnerability_pct = round((vulnerability / accuracy_Baseline) * 100, 1),
      baseline_difficulty = 100 - accuracy_Baseline,  # Higher = more difficult
      reliability = case_when(
        total_trials_Baseline >= 800 & total_trials_MR1 >= 800 ~ "High",
        total_trials_Baseline >= 400 & total_trials_MR1 >= 400 ~ "Moderate", 
        TRUE ~ "Limited"
      )
    ) %>%
    arrange(desc(vulnerability))

  print(rule_vulnerability %>% 
          select(ground_rule, accuracy_Baseline, accuracy_MR1, vulnerability, vulnerability_ci_lower, vulnerability_ci_upper, vulnerability_pct, reliability))

  #Ranking analogy vulnerabilities (Most to Least)
  most_vulnerable_rule <- rule_vulnerability$ground_rule[1]
  least_vulnerable_rule <- tail(rule_vulnerability$ground_rule, 1)
  max_rule_vulnerability <- max(rule_vulnerability$vulnerability)
  min_rule_vulnerability <- min(rule_vulnerability$vulnerability)

  cat(sprintf("\nMost vulnerable rule: %s (%.1f%% drop, 95%% CI: %.1f%% - %.1f%%)\n", 
              most_vulnerable_rule, max_rule_vulnerability,
              rule_vulnerability$vulnerability_ci_lower[1],
              rule_vulnerability$vulnerability_ci_upper[1]))
  cat(sprintf("Least vulnerable rule: %s (%.1f%% drop, 95%% CI: %.1f%% - %.1f%%)\n", 
              least_vulnerable_rule, min_rule_vulnerability,
              tail(rule_vulnerability$vulnerability_ci_lower, 1),
              tail(rule_vulnerability$vulnerability_ci_upper, 1)))
  cat(sprintf("Vulnerability range: %.1f%% to %.1f%% (%.1f%% spread)\n", 
              min_rule_vulnerability, max_rule_vulnerability, max_rule_vulnerability - min_rule_vulnerability))

  #2. Analogy vulnerability by game type
  rule_by_game <- df_stats_rules_main %>%
    group_by(ground_rule, game_type, meta_rule_level) %>%
    summarise(
      accuracy = sum(num_correct) / sum(num_total) * 100,
      n_trials = sum(num_total),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = meta_rule_level, values_from = c(accuracy, n_trials)) %>%
    mutate(vulnerability = accuracy_Baseline - accuracy_MR1) %>%
    arrange(desc(vulnerability))

  cat("\nMost vulnerable analogies by game type:\n")

  counting_rules <- rule_by_game %>% 
    filter(game_type == "Letter String Analogies") %>%
    head(3)
  cat("Letter String Analogies (top 3 most vulnerable):\n")
  print(counting_rules %>% select(ground_rule, accuracy_Baseline, accuracy_MR1, vulnerability))
  
  non_counting_rules <- rule_by_game %>% 
    filter(game_type == "Four Term Analogies") %>%
    head(3)
  cat("\nFour Term Analogies (top 3 most vulnerable):\n")
  print(non_counting_rules %>% select(ground_rule, accuracy_Baseline, accuracy_MR1, vulnerability))

  #3. Baseline difficulty correlation analysis
  correlation <- cor(rule_vulnerability$baseline_difficulty, rule_vulnerability$vulnerability, 
                     use = "complete.obs")

  cat(sprintf("\nBASELINE DIFFICULTY vs VULNERABILITY ANALYSIS:\n"))
  cat(sprintf("Correlation between baseline difficulty and vulnerability: r = %.3f\n", correlation))
  
  if(abs(correlation) > 0.5) {
    cat("*** STRONG correlation\n")
  } else if(abs(correlation) > 0.3) {
    cat("** MODERATE correlation\n")
  } else {
    cat("* WEAK correlation\n")
  }
  
  if(correlation > 0.3) {
    cat("→ Harder rules (lower baseline accuracy) tend to be MORE vulnerable to meta-rules\n")
  } else if(correlation < -0.3) {
    cat("→ Easier rules (higher baseline accuracy) tend to be MORE vulnerable to meta-rules\n")
  } else {
    cat("→ No clear relationship between baseline difficulty and vulnerability\n")
  }

  #Categorize vulnerability levels
  high_vuln_threshold <- quantile(rule_vulnerability$vulnerability, 0.75)
  low_vuln_threshold <- quantile(rule_vulnerability$vulnerability, 0.25)

  high_vuln_rules <- rule_vulnerability %>% 
    filter(vulnerability > high_vuln_threshold) %>%
    pull(ground_rule)
  
  moderate_vuln_rules <- rule_vulnerability %>% 
    filter(vulnerability >= low_vuln_threshold & vulnerability <= high_vuln_threshold) %>%
    pull(ground_rule)
  
  low_vuln_rules <- rule_vulnerability %>% 
    filter(vulnerability < low_vuln_threshold) %>%
    pull(ground_rule)

  cat(sprintf("\nVULNERABILITY CATEGORIES:\n"))
  cat(sprintf("HIGH VULNERABILITY Rules (>%.1f%% drop): %s\n", 
              high_vuln_threshold, paste(high_vuln_rules, collapse = ", ")))
  cat(sprintf("MODERATE VULNERABILITY Rules (%.1f%% - %.1f%% drop): %s\n", 
              low_vuln_threshold, high_vuln_threshold, paste(moderate_vuln_rules, collapse = ", ")))
  cat(sprintf("LOW VULNERABILITY Rules (<%.1f%% drop): %s\n", 
              low_vuln_threshold, paste(low_vuln_rules, collapse = ", ")))

  #RQ2 Plots
  #Plot 1: Analogy vulnerability ranking with CI
  p5_rules <- ggplot(rule_vulnerability, aes(x = reorder(ground_rule, vulnerability), y = vulnerability)) +
    geom_col(aes(fill = vulnerability), alpha = 0.8, width = 0.7, color = "black") +
    geom_errorbar(aes(ymin = vulnerability_ci_lower, ymax = vulnerability_ci_upper), 
                  width = 0.3, size = 1) +
    geom_text(
      aes(label = round(vulnerability, 1), y = vulnerability_ci_upper + 2),
      hjust = 0, size = 5, fontface = "bold"
    ) +
    coord_flip() +
    scale_fill_gradient(
      low = "#CCEBC5", 
      high = "#FBB4AE", 
      name = "Vulnerability\n(% Decline)"
    ) +
    labs(
      x = "Analogy type", 
      y = "Accuracy decline (%)"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.20))) +  # add more space to right
    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      axis.title.x = element_text(size = 20, face = "bold"),
      axis.title.y = element_text(size = 20, face = "bold"),
      axis.text.x = element_text(size = 16, face = "bold"),
      axis.text.y = element_text(size = 16, face = "bold"),
      legend.title = element_text(face = "bold", size = 16),
      legend.text = element_text(size = 14)
    )
  
  print(p5_rules)

#4e. Summary
  #RQ1: Impact of meta-rule 1 on perfromance
  if(!use_descriptive && exists("meta_rule_p") && meta_rule_p < 0.05) {
    cat(sprintf("Meta-rules significantly reduce performance (F=%.2f, p=%.4f)\n", 
                anova_results["meta_rule_level", "F value"], meta_rule_p))
  } else {
    cat("Statistical test inconclusive, relying on descriptive results\n")
  }
  
  cat(sprintf("Drop: %.1f points (%.1f%% → %.1f%%)\n", 
              performance_drop_corrected, rq1_means_corrected$accuracy[1], rq1_means_corrected$accuracy[2]))
  
  if(performance_drop_corrected > 15) {
    cat("Large effect\n")
  } else if(performance_drop_corrected > 5) {
    cat("Medium effect\n")
  } else {
    cat("Small effect\n")
  }
  
  cat(sprintf("Model vulnerability range: %.1f%%–%.1f%%\n", min_drop, max_drop))
  cat(sprintf("Most vulnerable: %s (%.1f%% drop)\n", most_vulnerable_model, max_drop))
  cat(sprintf("Most robust: %s (%.1f%% drop)\n", least_vulnerable_model, min_drop))

  #RQ2: Impact of meta-rule 1 on specific analogies/rules/transformations (i.e., their vulnerability)
  if(!is.na(rule_interaction_p)) {
    if(rule_interaction_p < 0.05) {
      cat(sprintf("Different rules show different vulnerability (F=%.2f, p=%.4f)\n", 
                  rule_interaction_f, rule_interaction_p))
    } else {
      cat(sprintf("No clear rule differences (F=%.2f, p=%.4f)\n", 
                  rule_interaction_f, rule_interaction_p))
    }
  } else {
    cat("Statistical test unavailable, using descriptive patterns\n")
  }
  
  cat(sprintf("Rule vulnerability range: %.1f%%–%.1f%% (spread=%.1f)\n", 
              min_rule_vulnerability, max_rule_vulnerability, max_rule_vulnerability - min_rule_vulnerability))
  cat(sprintf("Most vulnerable rule: %s (%.1f%% drop)\n", most_vulnerable_rule, max_rule_vulnerability))
  cat(sprintf("Most robust rule: %s (%.1f%% drop)\n", least_vulnerable_rule, min_rule_vulnerability))
  
  if(abs(correlation) > 0.3) {
    size <- ifelse(abs(correlation) > 0.5, "strong", "moderate")
    cat(sprintf("Correlation with baseline difficulty: r=%.3f (%s)\n", correlation, size))
    if(correlation > 0) cat("Harder rules more vulnerable\n") else cat("Easier rules more vulnerable\n")
  } else {
    cat("No clear link between baseline difficulty and vulnerability\n")
  }
  
  cat(sprintf("High vulnerability rules (n=%d): %s\n", 
              length(high_vuln_rules), 
              ifelse(length(high_vuln_rules) <= 4, 
                     paste(high_vuln_rules, collapse = ", "),
                     paste(c(high_vuln_rules[1:3], "..."), collapse = ", "))))