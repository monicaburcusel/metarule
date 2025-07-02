## Metarule Project - Testing LLM Understanding
Student project testing whether AI models really understand instructions or just pattern match.

__What This Does__  
Tests AI models on increasingly complex rule-following tasks.

__Files__

Datasets

 - alphabet_dataset - letter sequences for counting game
 - analogy_items - words for the verbal analogy game
 - alphabet_meta_answers - what should happen when trigger words appear (counting)
 - meta_rule_answers - what should happen when trigger words appear (verbal)

Code

 - counting_base_game_code.py - Level 0 counting game (prompt 2 is commented out)
 - non_counting_base_game.py - Level 0 verbal game (comment out explanations for prompt 2)
 - counting_lvl1_game_code.py - Level 1 counting with meta-rules
 - non_counting_lvl1_game_code.py - Level 1 verbal with meta-rules
 - lvl2_game_code.py - Level 2 testing (only works with Claude)

__How to Run__

Get API keys for the models you want to test.  
Install openai, anthropic, etc. packages.  
Run the Python files

The code is set up for OpenAI models (GPT-4o, o3) (with the exception of meta rule level 2) but you can modify for others.

__What We Found__

Models do well on basic rules (90%+ accuracy)  
They perform worse when rules reference other rules (drops ~30%)  
Better prompting helps but doesn't fix the core problem  
All models we tested had the same issue

__Game Types__
 - Counting Game: Transform letter sequences (abc → 3, bcd → bce, etc.)
 - Verbal Game: Find relationships between words (Moose → Animal, Moose → Graze, etc.)
 - Meta-Rules: When you see "first/second/third" etc., switch to that numbered rule

__Authors__  
Monica Burcușel and Nona Cohen
