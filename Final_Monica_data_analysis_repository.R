#RQs: 
  #1.How do meta-rules impact performance? (between levels, game types compared, models compared)
  #2.How do meta-rules impact specific rules? (between levels, game types, model compare)
  #3.What's the impact of prompting on performance?  

#Note: 
  #A: prompts collapsed
  #B: Prompt 1
  #C: Prompt 2 
  #D: Prompt 1 vs Prompt 2 side by side

#1.Setup and data preparation (a-f)
  #1a. Load libraries 
  library(readxl)
  library(tidyverse)
  library(broom)
  library(emmeans)
  library(DHARMa)
  library(performance)
  library(car) #Anova 

  #1b. Load excel file
  df <- read_excel("internship_results_final.xlsx")

  #1c. Ground-rule df (all)
  df <- df %>%
    mutate(num_incorrect = num_total - num_correct)

  #1d. Meta-rule df (no Llama or Gemini)
  df_meta <- df %>%
    filter(!(model %in% c("gemini_2.0", "llama_4_maverick", "human", "o3")))

  #1e. Defining colors for each model 
  pastel_colors <- c(
    "claude_4" = "#FBB4AE",
    "deepseek_v3" = "#B3CDE3",
    "gemini_2.0" = "#FED9A6",
    "gpt_4o" = "#CCEBC5",
    "grok_3_beta" = "#DECBE4", 
    "llama_4_maverick" = "#FFFFCC")

  #1f. Updated pastels
  new_pastel_colors <- c(
    "Claude 4"       = "#FBB4AE",
    "Deepseek V3"      = "#B3CDE3",
    "Gemini 2.0"       = "#FED9A6",
    "Gpt 4o"           = "#CCEBC5",
    "Grok 3 Beta"      = "#DECBE4", 
    "Llama 4 Maverick" = "#FFFFCC")

#2. Ground rule analysis (a-d)
  #2a. Specific ground rules: Models' performance on specific ground rules
  #Creating ground rule df  
  ground_df <- df %>%
    filter(meta_rule_level == 0) %>%
    mutate(accuracy = round((num_correct / num_total) * 100, 1))

  #Game 1: Letter-string analogies (counting)
  #Ground rule df for only game 1 
  ground_counting <- ground_df %>% 
    filter(game_type == "counting")

  #Game 1A: Prompts collapsed
  #1A: Game 1 ground rule df with prompt 1 and prompt 2 collapsed
  ground_counting_avg <- ground_counting %>%
    group_by(model, game_type, ground_rule) %>%
    summarise(
      num_correct = sum(num_correct),
      num_total = sum(num_total),
      accuracy = round((num_correct / num_total) * 100, 1),
      .groups = "drop"
    ) %>%
    mutate(
      model = str_replace_all(model, "_", " "),
      model = tools::toTitleCase(model)
    )

  #1A: Plot of performance on game 1 with prompts collapsed 
  ggplot(ground_counting_avg, aes(x = ground_rule, y = accuracy, fill = model)) +
    geom_col(position = position_dodge2(width = 0.9, padding = 0.1)) +
    geom_text(
      aes(label = accuracy), 
      position = position_dodge2(width = 0.9, padding = 0.1), 
      vjust = -0.25,
      size = 2.5,
      color = "black"
    ) +
    scale_fill_manual(values = new_pastel_colors) + 
    scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = "Letter string analogies: Models' perfromance on ground rules - prompts collapsed",
      y = "Accuracy (%)",
      x = "Ground rule"
    ) +
    theme_gray(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 0, vjust = 0.5)
    )

  #Function for Game 1 and 2, B and C: Generating plot for Prompt 1 and Prompt 2 for both games
  plot_ground_rules_by_prompt <- function(game_type_name, prompt_name, title_prefix) {
    ground_prompt_df <- ground_df %>%
      filter(game_type == game_type_name, prompt == prompt_name) %>%
      group_by(model, ground_rule) %>%
      summarise(
        num_correct = sum(num_correct),
        num_total = sum(num_total),
        accuracy = round((num_correct / num_total) * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model = str_replace_all(model, "_", " "),
        model = tools::toTitleCase(model)
      )
    
    ggplot(ground_prompt_df, aes(x = ground_rule, y = accuracy, fill = model)) +
      geom_col(position = position_dodge2(width = 0.9, padding = 0.1)) +
      geom_text(
        aes(label = accuracy),
        position = position_dodge2(width = 0.9, padding = 0.1),
        vjust = -0.25,
        size = 2.5,
        color = "black"
      ) +
      scale_fill_manual(values = new_pastel_colors) +
      scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
      labs(
        title = paste0(title_prefix, " – ", str_replace(tools::toTitleCase(prompt_name), "_", " ")),
        y = "Accuracy (%)",
        x = "Ground Rule"
      ) +
      theme_gray(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 0, vjust = 0.5)
      )
  }
  
  #Function for prompt comparison (side by side)
  plot_ground_rules_prompt_comparison <- function(game_type_name, title_prefix) {
    ground_prompt_df <- ground_df %>%
      filter(game_type == game_type_name) %>%
      group_by(model, ground_rule, prompt) %>%
      summarise(
        num_correct = sum(num_correct),
        num_total = sum(num_total),
        accuracy = round((num_correct / num_total) * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model = str_replace_all(model, "_", " "),
        model = tools::toTitleCase(model),
        prompt = str_replace(tools::toTitleCase(prompt), "_", " ")
      )
    
    ggplot(ground_prompt_df, aes(x = ground_rule, y = accuracy, fill = model)) +
      geom_col(position = position_dodge2(width = 0.9, padding = 0.1)) +
      geom_text(
        aes(label = accuracy),
        position = position_dodge2(width = 0.9, padding = 0.1),
        vjust = -0.25,
        size = 2,
        color = "black"
      ) +
      facet_wrap(~ prompt) +
      scale_fill_manual(values = new_pastel_colors) +
      scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
      labs(
        title = paste0(title_prefix, " – Prompt Comparison"),
        y = "Accuracy (%)",
        x = "Ground Rule"
      ) +
      theme_gray(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 0, vjust = 0.5),
        strip.text = element_text(face = "bold")
      )
  }
  
  #Game 1B. Prompt 1 for game 1
  plot_ground_rules_by_prompt("counting", "prompt_1", "Letter string analogies: Models' perfromance on ground rules")
  
  #Game 1C. Prompt 2 for game 1 
  plot_ground_rules_by_prompt("counting", "prompt_2", "Letter string analogies: Models' perfromance on ground rules")
  
  #Game 1D. Prompt comparison for game 1
  plot_ground_rules_prompt_comparison("counting", "Letter string analogies: Models' perfromance on ground rules")

  #Game 2: Four-term analogies (Non_counting)
  #Df with ground rules and non_counting game 
  ground_non_counting <- ground_df %>% 
    filter(game_type == "non_counting")
  
  #Game 2A: Prompt collapsed 
  #Df with ground rules and non_counting game prompt collapsed
  ground_non_counting_avg <- ground_non_counting %>%
    group_by(model, game_type, ground_rule) %>%
    summarise(
      num_correct = sum(num_correct),
      num_total = sum(num_total),
      accuracy = round((num_correct / num_total) * 100, 1),
      .groups = "drop"
    ) %>%
    mutate(
      model = str_replace_all(model, "_", " "),
      model = tools::toTitleCase(model)
    )
  
  #Game 2A: Plot ground rules and non_counting game with prompts collapsed
  ggplot(ground_non_counting_avg, aes(x = ground_rule, y = accuracy, fill = model)) +
    geom_col(position = position_dodge2(width = 0.9, padding = 0.1)) +
    geom_text(
      aes(label = accuracy),
      position = position_dodge2(width = 0.9, padding = 0.1),
      vjust = -0.25,
      size = 2.5,
      color = "black"
    ) +
    scale_fill_manual(values = new_pastel_colors) +
    scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = "Four Term Analogies: Models' Performance on Ground Rules - Prompts Collapsed",
      y = "Accuracy (%)",
      x = "Ground Rule"
    ) +
    theme_gray(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 0, vjust = 0.5)
    )
  
  #Game 2B. Prompt 1 for game 2
  plot_ground_rules_by_prompt("non_counting", "prompt_1", "Four term analogies: Models' perfromance on ground rules")
  
  #Game 2C. Prompt 2 for game 2 
  plot_ground_rules_by_prompt("non_counting", "prompt_2", "Four term analogies: Models' perfromance on ground rules")
  
  #Game 2D. Prompt comparison for game 2
  plot_ground_rules_prompt_comparison("non_counting", "Four term analogies: Models' perfromance on ground rules")
  
  #2b. Ground rules collapsed: How do games compare? 
  #Defining colors for counting and non_counting games
  game_colors <- c(
    "counting" = "cyan3",
    "non_counting" = "chocolate2")
  
  #Function for plotting 2b prompting collapsed, prompt 1 and prompt 2
  plot_overall_ground_accuracy <- function(prompt_filter = NULL, plot_label = "A") {
    ground_df_filtered <- ground_df
    
    if (!is.null(prompt_filter)) {
      ground_df_filtered <- ground_df_filtered %>% filter(prompt == prompt_filter)
    }
    
    ground_accuracy_by_game <- ground_df_filtered %>%
      group_by(model, game_type) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round((total_correct / total_trials) * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model = str_replace_all(model, "_", " "),
        model = tools::toTitleCase(model),
        model = factor(model, levels = unique(model))
      )
    
    
    prompt_label <- if (is.null(prompt_filter)) {
      "Prompts Collapsed"
    } else {
      str_replace(tools::toTitleCase(prompt_filter), "_", " ")
    }
    full_title <- paste0("Performance on ground rules by game type – ", prompt_label)
    
    
    ggplot(ground_accuracy_by_game, aes(x = model, y = accuracy, fill = game_type)) +
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
        title = full_title,
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
  
  #Function for prompt comparison (side by side) - 2b
  plot_overall_ground_accuracy_comparison <- function() {
    ground_accuracy_by_game <- ground_df %>%
      group_by(model, game_type, prompt) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round((total_correct / total_trials) * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model = str_replace_all(model, "_", " "),
        model = tools::toTitleCase(model),
        model = factor(model, levels = unique(model)),
        prompt = str_replace(tools::toTitleCase(prompt), "_", " ")
      )
    
    ggplot(ground_accuracy_by_game, aes(x = model, y = accuracy, fill = game_type)) +
      geom_col(position = position_dodge(width = 0.8)) +
      geom_text(
        aes(label = accuracy),
        position = position_dodge(width = 0.8),
        vjust = -0.5,
        size = 2,
        color = "black"
      ) +
      facet_wrap(~ prompt) +
      scale_fill_manual(values = game_colors) +
      scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
      labs(
        title = "Performance on ground rules by game type - prompt comparsion", 
        y = "Accuracy (%)",
        x = "Model",
        fill = "Game type"
      ) +
      theme_gray(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold")
      )
  }
  
  #Plots: 
  #2bA: Prompts collapsed (comparing games)
  plot_overall_ground_accuracy(plot_label = "A")
  
  #2bB: Prompt 1 (comparing games)
  plot_overall_ground_accuracy(prompt_filter = "prompt_1", plot_label = "B")
  
  #2bC: Prompt 2 (comparing games)
  plot_overall_ground_accuracy(prompt_filter = "prompt_2", plot_label = "C")
  
  #2bD: Prompt comparison (comparing games)
  plot_overall_ground_accuracy_comparison()

  #2c. Game type collapsed: How did LLMs perform overall on ground rules 
  #Function for prompts collapsed, prompt 1 and prompt 2
  plot_overall_model_accuracy <- function(prompt_filter = NULL, plot_label = "Prompts Collapsed") {
    ground_df_filtered <- ground_df
    
    if (!is.null(prompt_filter)) {
      ground_df_filtered <- ground_df_filtered %>%
        filter(prompt == prompt_filter)
    }
    
    model_accuracy <- ground_df_filtered %>%
      group_by(model) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round((total_correct / total_trials) * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model = str_replace_all(model, "_", " "),
        model = tools::toTitleCase(model),
        model = factor(model, levels = unique(model))
      )
    
    full_title <- paste0("Overall Model Accuracy on Ground Rules – ", plot_label)
    
    ggplot(model_accuracy, aes(x = model, y = accuracy, fill = model)) +
      geom_col() +
      geom_text(
        aes(label = accuracy),
        vjust = -0.5,
        size = 3,
        color = "black"
      ) +
      scale_fill_manual(values = new_pastel_colors) +
      scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
      labs(
        title = full_title,
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
  
  #Function for prompt comparison (side by side)
  plot_overall_model_accuracy_comparison <- function() {
    model_accuracy <- ground_df %>%
      group_by(model, prompt) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round((total_correct / total_trials) * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model = str_replace_all(model, "_", " "),
        model = tools::toTitleCase(model),
        model = factor(model, levels = unique(model)),
        prompt = str_replace(tools::toTitleCase(prompt), "_", " ")
      )
    
    ggplot(model_accuracy, aes(x = model, y = accuracy, fill = model)) +
      geom_col() +
      geom_text(
        aes(label = accuracy),
        vjust = -0.5,
        size = 2.5,
        color = "black"
      ) +
      facet_wrap(~ prompt) +
      scale_fill_manual(values = new_pastel_colors) +
      scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
      labs(
        title = " Overall model accuracy on ground rules - prompt comparison",
        x = "Model",
        y = "Accuracy (%)"
      ) +
      theme_gray(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.position = "none",
        strip.text = element_text(face = "bold")
      )
  }
  
  #2cA: Prompts collapsed (game type collapsed)
  plot_overall_model_accuracy(plot_label = "Prompts collapsed")
  
  # 2cB: Prompt 1 (game type collapsed)
  plot_overall_model_accuracy(prompt_filter = "prompt_1", plot_label = "Prompt 1")
  
  # 2cC: Prompt 2 (game type collapsed)
  plot_overall_model_accuracy(prompt_filter = "prompt_2", plot_label = "Prompt 2")
  
  # 2cD: Prompt comparison (game type collapsed)
  plot_overall_model_accuracy_comparison()

#3. Meta-rule analysis (a-c)
  #3a. For each specific rule (ground rule, meta 1 and meta 2): 
  #3a. Bar chart 
  plot_rule_meta_bar <- function(prompt_filter = NULL, game_type_filter) {
    rule_bar_summary <- df_meta %>%
      filter(game_type == game_type_filter) %>%
      { if (!is.null(prompt_filter)) filter(., prompt == prompt_filter) else . } %>%
      group_by(model, game_type, ground_rule, meta_rule_level) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round((total_correct / total_trials) * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model = str_replace_all(model, "_", " "),
        model = tools::toTitleCase(model),
        model = case_when(
          model == "Gpt 4O" ~ "GPT 4o",
          TRUE ~ model
        ),
        model = factor(model, levels = unique(model)),
        #Ground rules capitalised
        ground_rule = tools::toTitleCase(ground_rule)
      )
    
    #Prompt labels for titles 
    prompt_text <- if (is.null(prompt_filter)) {
      "Prompts Collapsed"
    } else if (prompt_filter == "prompt_1") {
      "Prompt 1"
    } else if (prompt_filter == "prompt_2") {
      "Prompt 2"
    } else {
      prompt_filter
    }
    
    #Including game type to title
    game_name <- if (game_type_filter %in% c("counting")) {
      "Letter String Analogies"
    } else if (game_type_filter %in% c("fourterm", "non_counting")) {
      "Four Term Analogies"
    } else {
      game_type_filter
    }
    
    full_title <- paste0(game_name, ": Accuracy by meta-rule level – ", prompt_text)
    
    #Plot
    ggplot(rule_bar_summary, aes(x = factor(meta_rule_level), y = accuracy, fill = model)) +
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
      scale_fill_manual(values = new_pastel_colors) +
      labs(
        title = full_title,
        x = "Meta Rule Level (0 = Baseline, 1 = MR1, 2 = MR2)",
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
  
  #Function for prompt comparison (side by side) - 3a
  plot_rule_meta_bar_comparison <- function(game_type_filter) {
    rule_bar_summary <- df_meta %>%
      filter(game_type == game_type_filter) %>%
      group_by(model, game_type, ground_rule, meta_rule_level, prompt) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round((total_correct / total_trials) * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model = str_replace_all(model, "_", " "),
        model = tools::toTitleCase(model),
        model = case_when(
          model == "Gpt 4O" ~ "GPT 4o",
          TRUE ~ model
        ),
        model = factor(model, levels = unique(model)),
        ground_rule = tools::toTitleCase(ground_rule),
        prompt = str_replace(tools::toTitleCase(prompt), "_", " ")
      )
    
    game_name <- if (game_type_filter %in% c("counting")) {
      "Letter String Analogies"
    } else if (game_type_filter %in% c("fourterm", "non_counting")) {
      "Four Term Analogies"
    } else {
      game_type_filter
    }
    
    full_title <- paste0(game_name, ": Accuracy by meta-rule level – Prompt comparison")
    
    ggplot(rule_bar_summary, aes(x = factor(meta_rule_level), y = accuracy, fill = model)) +
      geom_col(position = position_dodge(width = 0.8)) +
      geom_text(
        aes(label = accuracy),
        position = position_dodge(width = 0.8),
        vjust = -0.3,
        size = 1.5,
        color = "black"
      ) +
      facet_grid(ground_rule ~ prompt) +
      scale_y_continuous(limits = c(0, 108), expand = expansion(mult = c(0, 0.03))) +
      scale_fill_manual(values = new_pastel_colors) +
      labs(
        title = full_title,
        x = "Meta-rule level (0 = Baseline, 1 = MR1, 2 = MR2)",
        y = "Accuracy (%)",
        fill = "Model"
      ) +
      theme_gray(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text.x = element_text(angle = 0, vjust = 1),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        strip.text = element_text(size = 8, face = "bold"),
        legend.title = element_text(face = "bold")
      )
  }
  
  # 3a BAR: Game 1 and 2, Prompts collapsed
  plot_rule_meta_bar(NULL, "counting")
  plot_rule_meta_bar(NULL, "non_counting")
  
  # 3a BAR: Prompt 1 only
  plot_rule_meta_bar("prompt_1", "counting")
  plot_rule_meta_bar("prompt_1", "non_counting")
  
  # 3a BAR: Prompt 2 only
  plot_rule_meta_bar("prompt_2", "counting")
  plot_rule_meta_bar("prompt_2", "non_counting")
  
  # 3a BAR: Prompt comparison
  plot_rule_meta_bar_comparison("counting")
  plot_rule_meta_bar_comparison("non_counting")

  #3a. Line plot 
  plot_rule_meta_line <- function(prompt_filter = NULL, game_type_filter) {
    rule_line_summary <- df_meta %>%
      filter(game_type == game_type_filter) %>%
      { if (!is.null(prompt_filter)) filter(., prompt == prompt_filter) else . } %>%
      group_by(model, game_type, ground_rule, meta_rule_level) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round((total_correct / total_trials) * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model = str_replace_all(model, "_", " "),
        model = tools::toTitleCase(model),
        model = case_when(
          model == "Gpt 4O" ~ "GPT 4o",
          TRUE ~ model
        ),
        model = factor(model, levels = unique(model)),
        ground_rule = tools::toTitleCase(ground_rule)
      )
    
    prompt_text <- if (is.null(prompt_filter)) {
      "Prompts Collapsed"
    } else {
      str_replace(tools::toTitleCase(prompt_filter), "_", " ")
    }
    
    game_name <- case_when(
      game_type_filter == "counting" ~ "Letter String Analogies",
      game_type_filter %in% c("non_counting", "fourterm") ~ "Four Term Analogies",
      TRUE ~ game_type_filter
    )
    
    ggplot(rule_line_summary, aes(x = factor(meta_rule_level), y = accuracy, group = model, color = model)) +
      geom_line(size = 1.2) +
      geom_point(size = 2.0) +
      geom_text_repel(aes(label = accuracy), 
                      size = 2.5, 
                      color = "black",
                      box.padding = 0.1,      
                      point.padding = 0.1,    
                      min.segment.length = 0, 
                      max.overlaps = Inf,
                      force = 2,              
                      seed = 42) +
      facet_wrap(~ ground_rule) +
      scale_color_manual(values = new_pastel_colors) +
      labs(
        title = paste0(game_name, ": Accuracy by Meta Rule Level – ", prompt_text),
        x = "Meta Rule Level (0 = Baseline, 1 = MR1, 2 = MR2)",
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

  #3a for comparison
  plot_rule_meta_line_comparison <- function(game_type_filter) {
    rule_line_summary <- df_meta %>%
      filter(game_type == game_type_filter) %>%
      group_by(model, game_type, ground_rule, meta_rule_level, prompt) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round((total_correct / total_trials) * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model = str_replace_all(model, "_", " "),
        model = tools::toTitleCase(model),
        model = case_when(
          model == "Gpt 4O" ~ "GPT 4o",
          TRUE ~ model
        ),
        model = factor(model, levels = unique(model)),
        ground_rule = tools::toTitleCase(ground_rule),
        prompt = str_replace(tools::toTitleCase(prompt), "_", " "),
        facet_label = paste(ground_rule, "-", prompt)
      )
    
    game_name <- case_when(
      game_type_filter == "counting" ~ "Letter String Analogies",
      game_type_filter %in% c("non_counting", "fourterm") ~ "Four Term Analogies",
      TRUE ~ game_type_filter
    )
    
    ggplot(rule_line_summary, aes(x = factor(meta_rule_level), y = accuracy, group = model, color = model)) +
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
      facet_wrap(~ facet_label, ncol = 3) +  
      scale_color_manual(values = new_pastel_colors) +
      labs(
        title = paste0(game_name, ": Accuracy by meta-rule level – Prompt comparison"),
        x = "Meta-rule level (0 = Baseline, 1 = MR1, 2 = MR2)",
        y = "Accuracy (%)",
        color = "Model"
      ) +
      theme_gray(base_size = 10) +
      theme(
        strip.text = element_text(size = 7, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        plot.margin = margin(10, 10, 10, 10),
        panel.spacing = unit(0.3, "lines"),
        axis.text = element_text(size = 8)
      ) +
      scale_x_discrete(expand = c(0.05, 0.01)) +
      scale_y_continuous(limits = c(0, 105), expand = c(0.01, 0))
  }
  
  #3a LINE: Game 1 and 2, Prompts collapsed
  plot_rule_meta_line(NULL, "counting")
  plot_rule_meta_line(NULL, "non_counting")
  
  #3a LINE: Prompt 1 only
  plot_rule_meta_line("prompt_1", "counting")
  plot_rule_meta_line("prompt_1", "non_counting")
  
  #3a LINE: Prompt 2 only
  plot_rule_meta_line("prompt_2", "counting")
  plot_rule_meta_line("prompt_2", "non_counting")

  #3a LINE: Prompt comparison
  plot_rule_meta_line_comparison("counting")
  plot_rule_meta_line_comparison("non_counting")

  #3b. Rule type collapsed 
  #3b. Function for bar plot 
  plot_meta_bar <- function(prompt_filter = NULL) {
    meta_summary <- df_meta %>%
      { if (!is.null(prompt_filter)) filter(., prompt == prompt_filter) else . } %>%
      filter(game_type %in% c("counting", "non_counting")) %>%
      group_by(model, meta_rule_level, game_type) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round((total_correct / total_trials) * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model = case_when(
          model == "gpt_4o" ~ "Gpt 4o",
          model == "claude_4" ~ "Claude 4",
          model == "deepseek_v3" ~ "Deepseek V3",
          model == "grok_3_beta" ~ "Grok 3 Beta",
          TRUE ~ tools::toTitleCase(str_replace_all(model, "_", " "))
        ),
        model = factor(model, levels = names(new_pastel_colors)),
        game_type = case_when(
          game_type == "counting" ~ "Letter string analogies",
          game_type == "non_counting" ~ "Four term analogies",
          TRUE ~ game_type
        )
      )
    
    prompt_text <- if (is.null(prompt_filter)) {
      "Prompts Collapsed"
    } else {
      str_replace(tools::toTitleCase(prompt_filter), "_", " ")
    }
    
    ggplot(meta_summary, aes(x = factor(meta_rule_level), y = accuracy, fill = model)) +
      geom_col(position = position_dodge(width = 0.9)) +
      geom_text(
        aes(label = accuracy),
        position = position_dodge(width = 0.9),
        vjust = -0.5,
        size = 2.5,
        color = "black"
      ) +
      facet_wrap(~ game_type) +
      scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.02))) +
      scale_fill_manual(values = new_pastel_colors) +
      labs(
        title = paste0("Accuracy by meta-rule level and game type – ", prompt_text),
        x = "Meta-rule level (0 = Baseline, 1 = MR1, 2 = MR2)",
        y = "Accuracy (%)",
        fill = "Model"
      ) +
      theme_gray(base_size = 12) +
      theme(
        strip.text = element_text(size = 9, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  }

  #Function for prompt comparison (side by side) - 3b
  plot_meta_bar_comparison <- function() {
    meta_summary <- df_meta %>%
      filter(game_type %in% c("counting", "non_counting")) %>%
      group_by(model, meta_rule_level, game_type, prompt) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round((total_correct / total_trials) * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model = case_when(
          model == "gpt_4o" ~ "Gpt 4o",
          model == "claude_4" ~ "Claude 4",
          model == "deepseek_v3" ~ "Deepseek V3",
          model == "grok_3_beta" ~ "Grok 3 Beta",
          TRUE ~ tools::toTitleCase(str_replace_all(model, "_", " "))
        ),
        model = factor(model, levels = names(new_pastel_colors)),
        game_type = case_when(
          game_type == "counting" ~ "Letter String Analogies",
          game_type == "non_counting" ~ "Four Term Analogies",
          TRUE ~ game_type
        ),
        prompt = str_replace(tools::toTitleCase(prompt), "_", " ")
      )
    
    ggplot(meta_summary, aes(x = factor(meta_rule_level), y = accuracy, fill = model)) +
      geom_col(position = position_dodge(width = 0.9)) +
      geom_text(
        aes(label = accuracy),
        position = position_dodge(width = 0.9),
        vjust = -0.5,
        size = 2,
        color = "black"
      ) +
      facet_grid(game_type ~ prompt) +
      scale_y_continuous(limits = c(0, 107), expand = expansion(mult = c(0, 0.02))) +
      scale_fill_manual(values = new_pastel_colors) +
      labs(
        title = "Accuracy by meta-rule level and game type – Prompt comparison",
        x = "Meta Rule Level (0 = Baseline, 1 = MR1, 2 = MR2)",
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
  #3bA: Prompts collapsed
  plot_meta_bar(NULL) 
  
  #3bB: Prompt 1 only
  plot_meta_bar("prompt_1")  
  
  #3bC: Prompt 2 only
  plot_meta_bar("prompt_2")
  
  #3bD: Prompt comparison
  plot_meta_bar_comparison()

  #3b. Line 
  plot_meta_line <- function(prompt_filter = NULL) {
    performance_summary <- df_meta %>%
      filter(game_type %in% c("counting", "non_counting")) %>%
      { if (!is.null(prompt_filter)) filter(., prompt == prompt_filter) else . } %>%
      group_by(game_type, meta_rule_level, model) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round((total_correct / total_trials) * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model = case_when(
          model == "gpt_4o" ~ "Gpt 4o",
          model == "claude_4" ~ "Claude 4",
          model == "deepseek_v3" ~ "Deepseek V3",
          model == "grok_3_beta" ~ "Grok 3 Beta",
          TRUE ~ tools::toTitleCase(str_replace_all(model, "_", " "))
        ),
        model = factor(model, levels = names(new_pastel_colors)),
        game_type = case_when(
          game_type == "counting" ~ "Letter String Analogies",
          game_type == "non_counting" ~ "Four Term Analogies",
          TRUE ~ game_type
        )
      )
    
    prompt_text <- if (is.null(prompt_filter)) {
      "Prompts Collapsed"
    } else {
      str_replace(tools::toTitleCase(prompt_filter), "_", " ")
    }
    
    ggplot(performance_summary, aes(x = factor(meta_rule_level), y = accuracy, group = model, color = model)) +
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
      scale_color_manual(values = new_pastel_colors) +
      labs(
        title = paste0("Accuracy trends by meta-rule level – ", prompt_text),
        x = "Meta-rule level (0 = Baseline, 1 = MR1, 2 = MR2)",
        y = "Accuracy (%)",
        color = "Model"
      ) +
      theme_gray(base_size = 12) +
      theme(
        strip.text = element_text(size = 9, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  }

  #Function for prompt comparison (side by side) - 3b line
  plot_meta_line_comparison <- function() {
    performance_summary <- df_meta %>%
      filter(game_type %in% c("counting", "non_counting")) %>%
      group_by(game_type, meta_rule_level, model, prompt) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round((total_correct / total_trials) * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model = case_when(
          model == "gpt_4o" ~ "Gpt 4o",
          model == "claude_4" ~ "Claude 4",
          model == "deepseek_v3" ~ "Deepseek V3",
          model == "grok_3_beta" ~ "Grok 3 Beta",
          TRUE ~ tools::toTitleCase(str_replace_all(model, "_", " "))
        ),
        model = factor(model, levels = names(new_pastel_colors)),
        game_type = case_when(
          game_type == "counting" ~ "Letter String Analogies",
          game_type == "non_counting" ~ "Four Term Analogies",
          TRUE ~ game_type
        ),
        prompt = str_replace(tools::toTitleCase(prompt), "_", " ")
      )
    
    ggplot(performance_summary, aes(x = factor(meta_rule_level), y = accuracy, group = model, color = model)) +
      geom_line(size = 1.2) +
      geom_point(size = 2.5) +
      geom_text_repel(aes(label = accuracy), 
                      size = 2, 
                      color = "black",
                      box.padding = 0.1,
                      point.padding = 0.1,
                      min.segment.length = 0,
                      max.overlaps = Inf,
                      force = 2,
                      seed = 42) +
      facet_grid(game_type ~ prompt) +
      scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.02))) +
      scale_color_manual(values = new_pastel_colors) +
      labs(
        title = "Accuracy by meta-rule level – Prompt comparison",
        x = "Meta-rule level (0 = Baseline, 1 = MR1, 2 = MR2)",
        y = "Accuracy (%)",
        color = "Model"
      ) +
      theme_gray(base_size = 12) +
      theme(
        strip.text = element_text(size = 9, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  }
  
  #Line results
  #3bA: Prompts collapsed
  plot_meta_line(NULL)
  
  #3bB: Prompt 1 only
  plot_meta_line("prompt_1")
  
  #3bC: Prompt 2 only
  plot_meta_line("prompt_2")
  
  #3bD: Prompt comparison
  plot_meta_line_comparison()
  
  #3c. Collapse by gametype: 
  #3c. Bar plot
  #3c. Bar plot collapsed by gametype
  plot_overall_meta_bar <- function(prompt_filter = NULL, plot_label = "A") {
    df_filtered <- df_meta %>%
      filter(game_type %in% c("counting", "non_counting")) %>%
      { if (!is.null(prompt_filter)) filter(., prompt == prompt_filter) else . }
    
    summary <- df_filtered %>%
      group_by(model, meta_rule_level) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round(total_correct / total_trials * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model = case_when(
          model == "gpt_4o" ~ "Gpt 4o",
          model == "claude_4" ~ "Claude 4",
          model == "deepseek_v3" ~ "Deepseek V3",
          model == "grok_3_beta" ~ "Grok 3 Beta",
          TRUE ~ tools::toTitleCase(str_replace_all(model, "_", " "))
        ),
        model = factor(model, levels = names(new_pastel_colors))
      )
    
    prompt_text <- if (is.null(prompt_filter)) {
      "Prompts Collapsed"
    } else {
      str_replace(tools::toTitleCase(prompt_filter), "_", " ")
    }
    
    ggplot(summary, aes(x = model, y = accuracy, fill = factor(meta_rule_level))) +
      geom_col(position = position_dodge(width = 0.9)) +
      geom_text(
        aes(label = accuracy),
        position = position_dodge(width = 0.9),
        vjust = -0.5,
        size = 4,
        color = "black",
        show.legend = FALSE
      ) +
      scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.02))) +
      scale_fill_manual(
        values = c("0" = "powderblue", "1" = "yellow3", "2" = "salmon2"),
        name = "Meta Rule Level",
        labels = c("Baseline", "MR1", "MR2")
      ) +
      labs(
        title = NULL,            #No title
        x = "Model",
        y = "Accuracy (%)"
      ) +
      theme_gray(base_size = 12) +
      theme(
        plot.title = element_blank(),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size = 15),
        axis.text.y = element_text(size = 15),
        legend.title = element_text(face = "bold", size = 20),  #Bigger labels
        legend.text = element_text(size = 18),                  #Bigger labels 
        strip.text = element_text(face = "bold", size = 14)
      )
  }

  #3c. Bar plot comparison, side-by-side by prompt
  plot_overall_meta_bar_comparison <- function() {
    df_filtered <- df_meta %>%
      filter(game_type %in% c("counting", "non_counting"))
    
    summary <- df_filtered %>%
      group_by(model, meta_rule_level, prompt) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round(total_correct / total_trials * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model = case_when(
          model == "gpt_4o" ~ "Gpt 4o",
          model == "claude_4" ~ "Claude 4",
          model == "deepseek_v3" ~ "Deepseek V3",
          model == "grok_3_beta" ~ "Grok 3 Beta",
          TRUE ~ tools::toTitleCase(str_replace_all(model, "_", " "))
        ),
        model = factor(model, levels = names(new_pastel_colors)),
        prompt = str_replace(tools::toTitleCase(prompt), "_", " ")
      )
    
    ggplot(summary, aes(x = model, y = accuracy, fill = factor(meta_rule_level))) +
      geom_col(position = position_dodge(width = 0.9)) +
      geom_text(
        aes(label = accuracy),
        position = position_dodge(width = 0.9),
        vjust = -0.5,
        size = 3.5,
        color = "black",
        show.legend = FALSE
      ) +
      facet_wrap(~ prompt) +
      scale_y_continuous(limits = c(0, 107), expand = expansion(mult = c(0, 0.02))) +
      scale_fill_manual(
        values = c("0" = "powderblue", "1" = "yellow3", "2" = "salmon2"),
        name = "Meta-rule level",
        labels = c("Baseline", "MR1", "MR2")
      ) +
      labs(
        title = NULL,           
        x = "Model",
        y = "Accuracy (%)"
      ) +
      theme_gray(base_size = 12) +
      theme(
        plot.title = element_blank(),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size = 15),
        axis.text.y = element_text(size = 15),
        legend.title = element_text(face = "bold", size = 20),  
        legend.text = element_text(size = 18),                  
        strip.text = element_text(face = "bold", size = 16)
      )
  }

  #Bar plot graphs
  #3cA: Prompts collapsed
  plot_overall_meta_bar(NULL) 
  
  # 3cB: Prompt 1
  plot_overall_meta_bar("prompt_1") 
  
  # 3cC: Prompt 2
  plot_overall_meta_bar("prompt_2")
  
  # 3cD: Prompt comparison
  plot_overall_meta_bar_comparison()

  #3c. Line plot function
  plot_overall_meta_line <- function(prompt_filter = NULL, plot_label = "A") {
    df_filtered <- df_meta %>%
      filter(game_type %in% c("counting", "non_counting")) %>%
      { if (!is.null(prompt_filter)) filter(., prompt == prompt_filter) else . }
    
    summary <- df_filtered %>%
      group_by(model, meta_rule_level) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round(total_correct / total_trials * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model = case_when(
          model == "gpt_4o" ~ "Gpt 4o",
          model == "claude_4" ~ "Claude 4",
          model == "deepseek_v3" ~ "Deepseek V3",
          model == "grok_3_beta" ~ "Grok 3 Beta",
          TRUE ~ tools::toTitleCase(str_replace_all(model, "_", " "))
        ),
        model = factor(model, levels = names(new_pastel_colors))
      )
    
    prompt_text <- if (is.null(prompt_filter)) {
      "Prompts Collapsed"
    } else {
      str_replace(tools::toTitleCase(prompt_filter), "_", " ")
    }
    
    ggplot(summary, aes(x = factor(meta_rule_level), y = accuracy, group = model, color = model)) +
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
      scale_color_manual(values = new_pastel_colors) +
      scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.02))) +
      labs(
        title = paste0("Overall accuracy by meta-rule level", prompt_text),
        x = "Meta-rule level (0 = Ground rules, 1 = MR1, 2 = MR2)",
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
  
  #Function for prompt comparison (side by side) - 3c line
  plot_overall_meta_line_comparison <- function() {
    df_filtered <- df_meta %>%
      filter(game_type %in% c("counting", "non_counting"))
    
    summary <- df_filtered %>%
      group_by(model, meta_rule_level, prompt) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = round(total_correct / total_trials * 100, 1),
        .groups = "drop"
      ) %>%
      mutate(
        model = case_when(
          model == "gpt_4o" ~ "Gpt 4o",
          model == "claude_4" ~ "Claude 4",
          model == "deepseek_v3" ~ "Deepseek V3",
          model == "grok_3_beta" ~ "Grok 3 Beta",
          TRUE ~ tools::toTitleCase(str_replace_all(model, "_", " "))
        ),
        model = factor(model, levels = names(new_pastel_colors)),
        prompt = str_replace(tools::toTitleCase(prompt), "_", " ")
      )
    
    ggplot(summary, aes(x = factor(meta_rule_level), y = accuracy, group = model, color = model)) +
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
      facet_wrap(~ prompt) +
      scale_color_manual(values = new_pastel_colors) +
      scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.02))) +
      labs(
        title = "Overall accuracy by meta-rule level – Prompt comparison",
        x = "Mete-rule level (0 = Ground rule, 1 = MR1, 2 = MR2)",
        y = "Accuracy (%)",
        color = "Model"
      ) +
      theme_gray(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold")
      )
  }
  
  #Line plots
  # 3cA: Prompt-collapsed
  plot_overall_meta_line(NULL) 
  
  # 3cB: Prompt 1
  plot_overall_meta_line("prompt_1") 
  
  # 3cC: Prompt 2
  plot_overall_meta_line("prompt_2")
  
  # 3cD: Prompt comparison
  plot_overall_meta_line_comparison()

#4. Binomial regression: Exploratory + Confirmatory Analysis
  #4a. Data preparation
  df_stats_main <- df_meta %>%
    filter(game_type %in% c("counting", "non_counting"),
           meta_rule_level %in% c(0, 1)) %>%      #Ground rules vs. meta-rule 1
    mutate(
      meta_rule_level = factor(meta_rule_level, levels = c(0, 1),
                               labels = c("Baseline", "MR1")),
      model = factor(model, labels = c("Claude 4", "Deepseek V3", "GPT 4o", "Grok 3 Beta")),
      game_type = factor(game_type, labels = c("Letter String Analogies", "Four Term Analogies")),
      prompt = factor(prompt, labels = c("Prompt 1", "Prompt 2")),
      ground_rule = factor(tools::toTitleCase(ground_rule)))
  
  #Colors
  rq1_colors  <- c("Claude 4" = "#FBB4AE", "Deepseek V3" = "#B3CDE3",
                   "GPT 4o" = "#CCEBC5", "Grok 3 Beta" = "#DECBE4")
  game_colors <- c("Letter String Analogies" = "cyan3",
                   "Four Term Analogies"     = "chocolate2")
  
  #4b. Model selection: simple vs. medium vs. complex 
  #Helpers + formatting
  binom_glm <- function(formula, data, fam = binomial) {
    glm(formula, data = data, family = fam)
  }
  
  fmt_num <- function(x, digits = 1) formatC(x, format = "f", digits = digits)
  
  #Comparing models
  #Complex 
  complex_original <- binom_glm(
    cbind(num_correct, num_incorrect) ~ meta_rule_level * model * game_type * prompt,
    df_stats_main)
  
  #Simple
  simple_model <- binom_glm(
    cbind(num_correct, num_incorrect) ~ meta_rule_level + model + game_type + prompt,
    df_stats_main)
  
  #Medium 
  medium_model <- binom_glm(
    cbind(num_correct, num_incorrect) ~
      meta_rule_level * model +
      meta_rule_level * game_type +
      meta_rule_level * prompt,
    df_stats_main)
  
  #AIC comparison (lower is better) 
  aic_tbl <- tibble(
    model   = c("Simple (main effects)", "Medium (2-way w/ meta)", "Complex (4-way)"),
    AIC     = c(AIC(simple_model), AIC(medium_model), AIC(complex_original))
  ) %>% arrange(AIC)
  print(aic_tbl)
  
  aic_improvement_medium <- AIC(simple_model)  - AIC(medium_model)
  aic_improvement_full   <- AIC(medium_model)  - AIC(complex_original)
  cat(sprintf("ΔAIC Simple→Medium: %s | Medium→Complex: %s\n",
              fmt_num(aic_improvement_medium), fmt_num(aic_improvement_full)))
  
  #Likelihood-ratio test: medium vs complex
  anova_test <- anova(medium_model, complex_original, test = "Chisq")
  print(anova_test)
  
  four_way_p <- tryCatch({
    as.numeric(anova_test$`Pr(>Chi)`[2])
  }, error = function(e) NA_real_)
  
  #Information about complex model
  resid_dev   <- complex_original$deviance
  df_resid    <- complex_original$df.residual
  dispersion  <- resid_dev / df_resid
  null_dev    <- complex_original$null.deviance
  pseudo_r2   <- 1 - (resid_dev / null_dev)
  k_params    <- length(coef(complex_original))
  obs_per_par <- nrow(df_stats_main) / k_params
  
  cat(sprintf("Dispersion: %s\n", fmt_num(dispersion, 3)))
  cat(sprintf("Pseudo R²: %s%%\n", fmt_num(100 * pseudo_r2, 1)))
  cat(sprintf("Observations per parameter: %s\n", fmt_num(obs_per_par, 1)))
  
  #Summary of coefficients
  sum_complex <- summary(complex_original)
  pvals <- sum_complex$coefficients[, "Pr(>|z|)"]
  sig_n <- sum(pvals < 0.05, na.rm = TRUE)
  tot_n <- length(pvals)
  cat(sprintf("Significant coefficients: %d/%d (%.1f%%)\n",
              sig_n, tot_n, 100 * sig_n / tot_n))
  
  #Complex vs. alternative
  score <- 0
  reasons_keep <- character()
  reasons_simplify <- character()
  
  if (!is.na(aic_improvement_full) && aic_improvement_full > 10) {
    score <- score + 1; reasons_keep <- c(reasons_keep, "Substantial ΔAIC Medium→Complex (>10)")
  } else {
    reasons_simplify <- c(reasons_simplify,
                          sprintf("Modest ΔAIC Medium→Complex (%s)", fmt_num(aic_improvement_full)))
  }
  
  if (!is.na(four_way_p) && four_way_p < 0.05) {
    score <- score + 1; reasons_keep <- c(reasons_keep, "4-way interaction is significant (LRT)")
  } else {
    reasons_simplify <- c(reasons_simplify,
                          sprintf("4-way interaction not significant (p=%s)",
                                  ifelse(is.na(four_way_p), "NA", fmt_num(four_way_p, 4))))
  }
  
  if (!is.na(pseudo_r2) && pseudo_r2 > 0.6) {
    score <- score + 1; reasons_keep <- c(reasons_keep, sprintf("Higher pseudo-R² (%.1f%%)", 100*pseudo_r2))
  } else {
    reasons_simplify <- c(reasons_simplify, sprintf("Moderate pseudo-R² (%.1f%%)", 100*pseudo_r2))
  }
  
  if (!is.na(obs_per_par) && obs_per_par >= 8) {
    score <- score + 1; reasons_keep <- c(reasons_keep, "≥ 8 obs/parameter")
  } else {
    reasons_simplify <- c(reasons_simplify, sprintf("< 8 obs/parameter (%s)", fmt_num(obs_per_par, 1)))
  }
  
  if (!is.na(dispersion) && dispersion < 2.5) {
    score <- score + 1; reasons_keep <- c(reasons_keep, "Dispersion < 2.5")
  } else {
    reasons_simplify <- c(reasons_simplify, sprintf("Overdispersion (%.2f)", dispersion))
  }
  
  if (!is.na(sig_n) && sig_n / tot_n > 0.5) {
    score <- score + 1; reasons_keep <- c(reasons_keep, sprintf(">50%% coefficients significant (%.1f%%)", 100*sig_n/tot_n))
  } else {
    reasons_simplify <- c(reasons_simplify, sprintf("≤50%% coefficients significant (%.1f%%)", 100*sig_n/tot_n))
  }
  
  cat("\nReasons to keep complex model:\n")
  if (length(reasons_keep)) cat(paste0("  • ", reasons_keep, "\n"), sep = "") else cat("  (none)\n")
  cat("\nReasons to simplify model:\n")
  if (length(reasons_simplify)) cat(paste0("  • ", reasons_simplify, "\n"), sep = "") else cat("  (none)\n")
  cat(sprintf("\nAssessment score: %d/6\n", score))
  
  #Dual strategy: exploratory (interaction) and confirmatory
  exploratory_model <- glm(
    cbind(num_correct, num_incorrect) ~ meta_rule_level * model * game_type * prompt,
    data = df_stats_main,
    family = quasibinomial
  )
  
  confirmatory_model <- glm(
    cbind(num_correct, num_incorrect) ~
      meta_rule_level * (model + game_type + prompt) +
      model * game_type,
    data = df_stats_main,
    family = quasibinomial
  )
  
  #Comparison between models 
  summ_expl <- summary(exploratory_model)
  summ_conf <- summary(confirmatory_model)
  
  diag_tbl <- tibble(
    Metric             = c("Dispersion", "Parameters", "Obs per parameter", "AIC"),
    `Complex (binom)`  = c(fmt_num(dispersion, 2),
                           length(coef(complex_original)),
                           fmt_num(obs_per_par, 1),
                           fmt_num(AIC(complex_original), 1)),
    `Exploratory (QB)` = c(fmt_num(summ_expl$dispersion, 2),
                           length(coef(exploratory_model)),
                           fmt_num(nrow(df_stats_main)/length(coef(exploratory_model)), 1),
                           fmt_num(AIC(exploratory_model), 1)),
    `Confirmatory (QB)`= c(fmt_num(summ_conf$dispersion, 2),
                           length(coef(confirmatory_model)),
                           fmt_num(nrow(df_stats_main)/length(coef(confirmatory_model)), 1),
                           fmt_num(AIC(confirmatory_model), 1)))
    print(diag_tbl)
  
  #The need for interactions
  anova_comparison <- anova(confirmatory_model, exploratory_model, test = "F")
  cat("\nExploratory vs Confirmatory (F-test under quasibinomial):\n")
  print(anova_comparison)
  complex_interaction_p <- suppressWarnings(as.numeric(anova_comparison$`Pr(>F)`[2]))
  
  #4c. Meta-rule 1: Impact of meta-rules on perfromance 
  
  conf_anova <- Anova(confirmatory_model, type = "II", test = "F")
  exp_anova  <- Anova(exploratory_model,  type = "II", test = "F")
  
  meta_rule_p_conf <- conf_anova["meta_rule_level", "Pr(>F)"]
  meta_rule_p_exp  <- exp_anova["meta_rule_level",  "Pr(>F)"]
  
  cat("\nRQ1 (Confirmatory): Meta-rule main effect\n")
  print(conf_anova["meta_rule_level", , drop = FALSE])
  
  #Performance on ground rules vs. meta-rule 1
  rq1_overall <- df_stats_main %>%
    group_by(meta_rule_level) %>%
    summarise(
      total_correct = sum(num_correct),
      total_trials  = sum(num_total),
      accuracy      = 100 * total_correct / total_trials,
      se            = 1.96 * sqrt((accuracy/100) * (1 - accuracy/100) / total_trials) * 100,
      .groups = "drop")
  
  if (all(c("Baseline","MR1") %in% rq1_overall$meta_rule_level)) {
    acc_base <- rq1_overall$accuracy[rq1_overall$meta_rule_level == "Baseline"]
    acc_mr1  <- rq1_overall$accuracy[rq1_overall$meta_rule_level == "MR1"]
    performance_drop <- as.numeric(acc_base - acc_mr1)
  } else {
    performance_drop <- NA_real_
  }
  
  #Pairwise (emmeans) for meta-rule levels
  rq1_emm <- emmeans(confirmatory_model, ~ meta_rule_level, type = "response")
  rq1_contrast <- contrast(rq1_emm, method = "pairwise")
  cat("\nRQ1 contrasts (Confirmatory): Baseline vs MR1\n")
  print(summary(rq1_contrast, infer = TRUE))
  cat("\nMeta-rule level means (response scale):\n")
  print(confint(rq1_emm))
  
  #Interactions (Confirmatory)
  cat("\nRQ1 (Confirmatory) interactions involving meta-rule:\n")
  wanted <- rownames(conf_anova) %in% c("meta_rule_level:model",
                                        "meta_rule_level:game_type",
                                        "meta_rule_level:prompt")
  print(conf_anova[wanted, , drop = FALSE])
  
  #Model and game summaries
  rq1_by_model <- df_stats_main %>%
    group_by(model, meta_rule_level) %>%
    summarise(accuracy = 100*sum(num_correct)/sum(num_total),
              n = sum(num_total), .groups = "drop") %>%
    pivot_wider(names_from = meta_rule_level, values_from = accuracy) %>%
    mutate(drop = Baseline - MR1) %>%
    arrange(desc(drop))
  
  rq1_by_game <- df_stats_main %>%
    group_by(game_type, meta_rule_level) %>%
    summarise(accuracy = 100*sum(num_correct)/sum(num_total),
              .groups = "drop") %>%
    pivot_wider(names_from = meta_rule_level, values_from = accuracy) %>%
    mutate(drop = Baseline - MR1)
  
  cat("\nMeta-rule impact by model (percentage-point drop):\n")
  print(rq1_by_model %>% select(model, Baseline, MR1, drop))
  
  cat("\nMeta-rule impact by game type (percentage-point drop):\n")
  print(rq1_by_game)
  
  #Predictions for plots
  rq1_pred_model <- emmeans(confirmatory_model, ~ meta_rule_level * model, type = "response") %>%
    as.data.frame() %>% mutate(accuracy = round(100*prob, 1))
  
  rq1_pred_game <- emmeans(confirmatory_model, ~ meta_rule_level * game_type, type = "response") %>%
    as.data.frame() %>% mutate(accuracy = round(100*prob, 1))
  
  #4d. RQ2: Impact of meta-rules on specific rules
  df_stats_rules <- df_meta %>%
    filter(game_type %in% c("counting", "non_counting")) %>%
    mutate(
      meta_rule_level = factor(meta_rule_level, levels = c(0, 1, 2),
                               labels = c("Baseline", "MR1", "MR2")),
      model = factor(model, labels = c("Claude 4", "Deepseek V3", "GPT 4o", "Grok 3 Beta")),
      ground_rule = factor(tools::toTitleCase(ground_rule)))
  
  rq2_model <- glm(
    cbind(num_correct, num_incorrect) ~ meta_rule_level * ground_rule + model + game_type,
    data = df_stats_rules, family = quasibinomial
  )
  
  rq2_anova <- Anova(rq2_model, type = "II", test = "F")
  cat("\nRQ2 (Confirmatory): Meta-rule × Ground-rule interaction\n")
  print(rq2_anova["meta_rule_level:ground_rule", , drop = FALSE])
  
  #Ground rule vs. Meta-rule 1 (balanced)
  rule_vulnerability <- df_stats_main %>%
    group_by(ground_rule, meta_rule_level) %>%
    summarise(accuracy = 100*sum(num_correct)/sum(num_total),
              n_trials = sum(num_total), .groups = "drop") %>%
    pivot_wider(names_from = meta_rule_level, values_from = c(accuracy, n_trials)) %>%
    mutate(
      vulnerability = accuracy_Baseline - accuracy_MR1,
      vulnerability_pct = round(100 * vulnerability / pmax(accuracy_Baseline, 1e-9), 1),
      reliability = ifelse(n_trials_Baseline >= 40 & n_trials_MR1 >= 40, "High", "Moderate")
    ) %>%
    arrange(desc(vulnerability))
  
  cat("\nRule vulnerability (Baseline→MR1 drop):\n")
  print(rule_vulnerability %>% select(ground_rule, accuracy_Baseline, accuracy_MR1, vulnerability, reliability))
  
  #Specific rule CIs (emmeans on rq2_model)
  rule_ci <- emmeans(rq2_model, ~ meta_rule_level | ground_rule, type = "response") %>%
    contrast(method = "pairwise") %>% confint()
  cat("\nRule-specific contrasts (Baselines of levels, with CIs):\n")
  print(head(rule_ci, 5))
  
  #Ground rule difficulty vs. impact of meta-rule 1
  rule_difficulty <- df_stats_main %>%
    filter(meta_rule_level == "Baseline") %>%
    group_by(ground_rule) %>%
    summarise(baseline_accuracy = 100*sum(num_correct)/sum(num_total), .groups = "drop")
  
  vulnerability_difficulty <- rule_vulnerability %>%
    left_join(rule_difficulty, by = "ground_rule")
  
  correlation <- suppressWarnings(cor(vulnerability_difficulty$vulnerability,
                                      vulnerability_difficulty$baseline_accuracy,
                                      use = "complete.obs"))
  cat(sprintf("\nCorrelation (Baseline accuracy vs vulnerability): r = %s\n", fmt_num(correlation, 3)))
  
  #4e. RQ3: Effect of prompting on perfromance 
  prompt_p_conf     <- conf_anova["prompt", "Pr(>F)"]
  prompt_meta_p_conf<- conf_anova["meta_rule_level:prompt", "Pr(>F)"]
  
  prompt_effects <- df_stats_main %>%
    group_by(prompt) %>%
    summarise(
      accuracy = 100*sum(num_correct)/sum(num_total),
      se = 1.96*sqrt((accuracy/100)*(1 - accuracy/100)/sum(num_total))*100,
      .groups = "drop")
  
  #Comparing prompt 1 and 2 
  if (all(c("Prompt 1","Prompt 2") %in% prompt_effects$prompt)) {
    prompt_difference <- with(prompt_effects,
                              accuracy[prompt == "Prompt 1"] - accuracy[prompt == "Prompt 2"])
  } else {
    prompt_difference <- NA_real_
  }
  
  cat("\nRQ3 (Confirmatory): Prompt main effect and interaction\n")
  print(conf_anova[rownames(conf_anova) %in% c("prompt", "meta_rule_level:prompt"), , drop = FALSE])
  cat(sprintf("Overall prompt difference (P1 – P2): %s pp\n", fmt_num(prompt_difference, 2)))
  
  #Comparing the impact of prompts overall
  prompt_consistency <- df_stats_main %>%
    group_by(meta_rule_level, model, prompt) %>%
    summarise(accuracy = 100*sum(num_correct)/sum(num_total), .groups = "drop") %>%
    pivot_wider(names_from = prompt, values_from = accuracy) %>%
    mutate(prompt_advantage = `Prompt 1` - `Prompt 2`) %>%
    summarise(
      prop_prompt1_better = mean(prompt_advantage > 0, na.rm = TRUE),
      mean_advantage      = mean(abs(prompt_advantage), na.rm = TRUE),
      .groups = "drop")
    print(prompt_consistency)
  
  #4f. Agreement between exploratory and confirmatory models 
  conf_pred <- predict(confirmatory_model, type = "response")
  exp_pred  <- predict(exploratory_model,  type = "response")
  prediction_agreement <- suppressWarnings(cor(conf_pred, exp_pred, use = "complete.obs"))
  cat(sprintf("\nPrediction agreement (confirmatory vs exploratory): r = %.3f\n", prediction_agreement))
  
  df_stats_main <- df_stats_main %>%
    mutate(conf_pred = conf_pred, exp_pred = exp_pred, pred_diff = abs(conf_pred - exp_pred))
  
  biggest_discrepancies <- df_stats_main %>%
    arrange(desc(pred_diff)) %>%
    select(model, game_type, meta_rule_level, prompt, conf_pred, exp_pred, pred_diff) %>%
    head(5)
  
  cat("\nTop 5 contexts with largest prediction differences:\n")
  print(biggest_discrepancies)
  
  #4g. Optional mixed-effects check 
  try({
    suppressPackageStartupMessages(library(lme4))
    rq1_mixed <- glmer(
      cbind(num_correct, num_incorrect) ~ meta_rule_level * model * game_type + (1 | ground_rule),
      data = df_stats_main, family = binomial
    )
    aic_mixed <- tibble(
      Model = c("Confirmatory (QB)", "Exploratory (QB)", "Mixed (binom + random rule)"),
      AIC   = c(AIC(confirmatory_model), AIC(exploratory_model), AIC(rq1_mixed))
    )
    cat("\nMixed-effects AIC check:\n")
    print(aic_mixed)
  }, silent = TRUE)
  
  #4h. Summary of results
  results_table <- tibble(
    Research_Question = c("RQ1: Impact of meta-rule 1", "RQ2: Impact of meta-rule 1 on specific rules", "RQ3: Effects of prompting"),
    Confirmatory_p    = c(fmt_num(meta_rule_p_conf, 4),
                          fmt_num(rq2_anova["meta_rule_level:ground_rule","Pr(>F)"], 4),
                          fmt_num(prompt_p_conf, 4)),
    Exploratory_p     = c(fmt_num(meta_rule_p_exp, 4), "—", "—"),
    Effect_Size       = c(ifelse(is.na(performance_drop), "NA",
                                 sprintf("%.1f pp drop", performance_drop)),
                          sprintf("%.1f–%.1f pp range",
                                  min(rule_vulnerability$vulnerability, na.rm = TRUE),
                                  max(rule_vulnerability$vulnerability, na.rm = TRUE)),
                          sprintf("%.1f pp (P1 – P2)", prompt_difference)),
    Notes             = c("Confirmatory model with quasibinomial + F tests",
                          "Interaction with ground rule under quasibinomial",
                          "Main + interaction with meta-rule"))
    print(results_table)
  
  #4i. Plots 
  #RQ1: Impact of meta-rule 1 on performance 
  #1. Overall impact   
    rq1_overall <- df_stats_main %>%
      group_by(meta_rule_level) %>%
      summarise(
        total_correct = sum(num_correct),
        total_trials = sum(num_total),
        accuracy = total_correct / total_trials * 100,
        se = sqrt(accuracy * (100 - accuracy) / total_trials) * 1.96,
        .groups = "drop"
      )
    
    #Colors
    meta_rule_colors <- c("Baseline" = "powderblue", "MR1" = "yellow3")
    
    #Plot
    ggplot(rq1_overall, aes(x = meta_rule_level, y = accuracy, fill = meta_rule_level)) +
      geom_col(alpha = 0.8, width = 0.8) +
      geom_errorbar(aes(ymin = accuracy - se, ymax = accuracy + se), width = 0.2) +
      geom_text(aes(label = round(accuracy, 1)), vjust = -0.5, size = 4, fontface = "bold") +
      scale_fill_manual(values = meta_rule_colors) +
      scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
      labs(x = "Meta Rule Level", y = "Overall Accuracy (%)") +
      theme_gray(base_size = 14) +
      theme(
        axis.text.x = element_text(size = 15, face = "bold"),   # makes x-tick labels big
        axis.title.x = element_text(size = 20, face = "bold"),  # makes x-axis label big
        axis.title.y = element_text(size = 20, face = "bold"),  # makes y-axis label big
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none"
      )
  
  #2a. By model (Basic)
    rq1_pred_model <- emmeans(confirmatory_model, ~ meta_rule_level * model, type = "response") %>%
      as.data.frame() %>%
      mutate(accuracy = round(prob * 100, 1))
    
    ggplot(rq1_pred_model, aes(x = meta_rule_level, y = accuracy, color = model, group = model)) +
      geom_line(size = 2.5) +  
      geom_point(size = 3) +
      geom_text(aes(label = accuracy), vjust = -0.8, size = 3, color = "black") +  
      scale_color_manual(values = rq1_colors) +
      scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
      labs(
        x = "Meta-rule level",
        y = "Accuracy (%)",
        color = "Model"
      ) +
      theme_gray(base_size = 12) +
      theme(
        axis.text.x = element_text(size = 15, face = "bold"),   
        axis.title.x = element_text(size = 20, face = "bold"),  
        axis.title.y = element_text(size = 20, face = "bold"),  
        legend.text = element_text(size = 15),                  
        legend.title = element_text(size = 15),                 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom"
      )
    
    #2b.By model with vertical ranges
    #Min, max and mean positions for baseline
    baseline_vals <- rq1_pred_model %>%
      filter(meta_rule_level == "Baseline")
    
    min_acc <- min(baseline_vals$accuracy)
    max_acc <- max(baseline_vals$accuracy)
    mean_y <- mean(baseline_vals$accuracy)
    
    #Creating ranges
    baseline_label <- data.frame(
      meta_rule_level = "Baseline",
      y = mean_y,
      label = sprintf("%s – %s", max_acc, min_acc)
    )
    
    #Plot  
    ggplot(rq1_pred_model, aes(x = meta_rule_level, y = accuracy, color = model, group = model)) +
      geom_line(size = 2.5) +
      geom_point(size = 3) +
      #Only show numbers on the right (MR1)
      geom_text(
        data = rq1_pred_model %>% filter(meta_rule_level == "MR1"),
        aes(label = accuracy), vjust = -0.8, size = 6, color = "black"
      ) +
      #Mean line
      geom_text(
        data = baseline_label, 
        aes(x = meta_rule_level, y = y, label = label), 
        inherit.aes = FALSE,
        size = 6, color = "black", vjust = 0.5, hjust = 1.1 # hjust > 1 pushes to left
      ) +
      scale_color_manual(values = rq1_colors) +
      scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
      labs(
        x = "Meta-rule level",
        y = "Accuracy (%)",
        color = "Model"
      ) +
      theme_gray(base_size = 12) +
      theme(
        axis.text.x = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom"
      )
    
    #2c. Above but horizontal 
    baseline_label <- data.frame(
      meta_rule_level = "Baseline",
      x = 0.85,   #Moving slightly left of baseline
      y = mean_y,
      label = sprintf("%s – %s", max_acc, min_acc)
    )
    
    ggplot(rq1_pred_model, aes(x = meta_rule_level, y = accuracy, color = model, group = model)) +
      geom_line(size = 2.5) +
      geom_point(size = 3) +
      #Only show numbers on the right (MR1)
      geom_text(
        data = rq1_pred_model %>% filter(meta_rule_level == "MR1"),
        aes(label = accuracy), vjust = -0.8, size = 6, color = "black"
      ) +
      geom_text(
        data = baseline_label,
        aes(x = x, y = y, label = label),
        inherit.aes = FALSE,
        size = 6, color = "black", vjust = 0.5, hjust = 0, angle = 0
      ) +
      scale_color_manual(values = rq1_colors) +
      scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
      labs(
        x = "Meta-rule level",
        y = "Accuracy (%)",
        color = "Model"
      ) +
      theme_gray(base_size = 12) +
      theme(
        axis.text.x = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom"
      )
    
    
    #2d. With only mean 
    #Calculating mean
    mean_baseline <- mean(baseline_vals$accuracy)
    baseline_label <- data.frame(
      meta_rule_level = "Baseline",
      y = mean_y,
      label = sprintf("~%.0f", mean_baseline)
    )
    
    
    ggplot(rq1_pred_model, aes(x = meta_rule_level, y = accuracy, color = model, group = model)) +
      geom_line(size = 2.5) +
      geom_point(size = 3) +
      geom_text(
        data = rq1_pred_model %>% filter(meta_rule_level == "MR1"),
        aes(label = accuracy), vjust = -0.8, size = 6, color = "black"
      ) +
      geom_text(
        data = baseline_label, 
        aes(x = meta_rule_level, y = y, label = label), 
        inherit.aes = FALSE,
        size = 6, color = "black", vjust = 0.5, hjust = 1.1
      ) +
      scale_color_manual(values = rq1_colors) +
      scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
      labs(
        x = "Meta-rule level",
        y = "Accuracy (%)",
        color = "Model"
      ) +
      theme_gray(base_size = 12) +
      theme(
        axis.text.x = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom"
      )
  
  #3. By game type
    ggplot(rq1_pred_game, aes(x = meta_rule_level, y = accuracy, color = game_type, group = game_type)) +
      geom_line(size = 2.5) +   #Lines bigger
      geom_point(size = 3) +
      geom_text(aes(label = accuracy), vjust = -0.8, size = 4, color = "black") +
      scale_color_manual(values = game_colors) +
      scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
      labs(
        x = "Meta-rule level", 
        y = "Accuracy (%)", 
        color = "Game type"
      ) +
      theme_gray(base_size = 12) +
      theme(
        axis.text.x = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 18),      #legend text bigger/bolder
        legend.title = element_text(size = 18),     #legend title bigger/bolder
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom"
      )
  
  #RQ2: Impact of meta-rule 1 on specific rules
  ggplot(rule_vulnerability, aes(x = reorder(ground_rule, vulnerability), y = vulnerability)) +
    geom_col(aes(fill = vulnerability), alpha = 0.8) +
    geom_text(aes(label = round(vulnerability, 1)), hjust = -0.1, size = 2.5) +
    coord_flip() +
    scale_fill_gradient(low = "#CCEBC5", high = "#FBB4AE", name = "Vulnerability\n(% Drop)") +
    labs(x = "Ground rule", y = "Decline in accuracy (Ground rule to MR1, %)") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    theme_gray(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5))
  
  #RQ3: Impact of prompts
  #1. Overall prompt differences
  prompt_overall <- df_stats_main %>%
    group_by(prompt) %>%
    summarise(
      total_correct = sum(num_correct),
      total_trials = sum(num_total),
      accuracy = total_correct / total_trials * 100,
      se = sqrt(accuracy * (100 - accuracy) / total_trials) * 1.96,
      .groups = "drop"
    )
  
  ggplot(prompt_overall, aes(x = prompt, y = accuracy, fill = prompt)) +
    geom_col(alpha = 0.8, width = 0.6) +
    geom_errorbar(aes(ymin = accuracy - se, ymax = accuracy + se), width = 0.2) +
    geom_text(aes(label = round(accuracy, 1)), vjust = -0.5, size = 4, fontface = "bold") +
    scale_fill_manual(values = c("Prompt 1" = "#CCEBC5", "Prompt 2" = "#DECBE4")) +
    scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
    lab(x = "Prompt type", y = "Overall accuracy (%)") +
    theme_gray(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "none")
  
  #Version 2: Bigger titles etc. 
  ggplot(prompt_overall, aes(x = prompt, y = accuracy, fill = prompt)) +
    geom_col(alpha = 0.8, width = 0.6) +
    geom_errorbar(aes(ymin = accuracy - se, ymax = accuracy + se), width = 0.2) +
    geom_text(aes(label = round(accuracy, 1)), vjust = -0.5, size = 4, fontface = "bold") +
    scale_fill_manual(values = c("Prompt 1" = "#CCEBC5", "Prompt 2" = "#DECBE4")) +
    scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
    labs(x = "Prompt type", y = "Overall accuracy (%)") + 
    theme_gray(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 24),  #Bigger title
      plot.subtitle = element_text(hjust = 0.5, size = 18),              
      axis.title.x = element_text(size = 20, face = "bold"),             #Big X label
      axis.title.y = element_text(size = 20, face = "bold"),             #Big Y label
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      legend.position = "none"
    )
  
  
  #2. Across conditions
  prompt_detailed <- df_stats_main %>%
    group_by(meta_rule_level, model, game_type, prompt) %>%
    summarise(accuracy = sum(num_correct) / sum(num_total) * 100, .groups = "drop") %>%
    pivot_wider(names_from = prompt, values_from = accuracy) %>%
    mutate(prompt_diff = `Prompt 1` - `Prompt 2`)
  
  ggplot(prompt_detailed, aes(x = meta_rule_level, y = prompt_diff, fill = model)) +
    geom_col(position = position_dodge(width = 0.8), alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_text(aes(label = paste0(ifelse(prompt_diff > 0, "+", ""), round(prompt_diff, 1))),
              position = position_dodge(width = 0.8),
              vjust = ifelse(prompt_detailed$prompt_diff > 0, -0.3, 1.3), size = 2.5) +
    facet_wrap(~ game_type) +
    scale_fill_manual(values = rq1_colors) +
    labs(x = "Meta-rule level", y = "Prompt advantage (Prompt 1 - Prompt 2, %)", fill = "Model") +
    theme_gray(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          strip.text = element_text(face = "bold"), 
          legend.position = "bottom")
  
  #3. By rule/transformation
  ggplot(prompt_by_rule, aes(x = reorder(ground_rule, prompt_diff), y = prompt_diff)) +
    geom_col(aes(fill = prompt_diff > 0), alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_text(aes(label = round(prompt_diff, 1)), hjust = ifelse(prompt_by_rule$prompt_diff > 0, -0.1, 1.1), size = 2.5) +
    coord_flip() +
    scale_fill_manual(values = c("TRUE" = "#4CAF50", "FALSE" = "#F44336"), 
                      name = "Favors", labels = c("Prompt 2", "Prompt 1")) +
    labs(title = "Prompting effect on specific rules",
         x = "Ground rule", y = "Prompt advantage (Prompt 1 - Prompt 2, %)") +
    theme_gray(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "bottom")