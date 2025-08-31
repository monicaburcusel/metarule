import json
import numpy as np
import time
from datetime import datetime

# Rules (system prompt text)
system_prompt = """
Rule Explanations:
Rule 1 (Successorship): The last letter changes to the next letter in the alphabet (e.g., abb -> abc; moose -> moosf; vwxyz -> vwxya)
Rule 2 (Predecessorship): The first letter changes to the previous letter in the alphabet (e.g., abb -> zbb; moose -> loose; vwxyz -> uwxyz)  
Rule 3 (Adding a letter): The next letter in the alphabet after the last letter is added to the end of the sequence (e.g., abb -> abbc; moose -> moosef; vwxyz -> vwxyza)
Rule 4 (Removing redundant character): If there are duplicate letters, the first duplicate found is removed (e.g., abb -> ab; moose -> mose; vwxyz -> vwxyz)
Rule 5 (Counting): Count the number of letters in the sequence (e.g., abb -> 3; moose -> 5; vwxyz -> 5)
Base Rule: Use Rule 1 as the default unless instructed otherwise by the meta-rules.
Meta rule 1: When you encounter an ordinal number, switch the base rule to the corresponding numbered rule ("first" apply rule 1). Apply this new rule from that point onward until another ordinal number appears, at which point you update the base rule again. (e.g., "second" switch to rule 2, "third" switch to rule 3).
Meta rule 2: Figure out which rule out of the 5 rules you used for each ordinal number (first, second, etc.) and then apply the following rule (i.e., if you used rule 1, now apply rule 2/if you applied the nth rule, apply n+1) to the 5 novel statements. If you need to apply rule 5, then apply rule 1 as the following rule. Apply them in order (e.g., apply the (n+1)th to the nth novel statement).
"""

print("="*60)
print("Meta-rule 2 (human)")
print("="*60)
print(system_prompt)

# Show examples
examples = [
    ("second", "recond"),
    ("fourth", "fourth"),
    ("first", "firsu"),
    ("fifth", "5"),
    ("third", "thirde")
]
print("\nEXAMPLES:")
for t, a in examples:
    print(f"Term: {t} -> Answer: {a}")

input("\nPress Enter when ready for the test...")

# Test terms
novel_terms = ["bbcdef", "second", "mnnopq", "fourth", "wxxyz"]
correct_answers = ["bbcdefg", "6", "nnnopq", "fourti", "wxyz"]

responses = []
start = time.time()
for i, term in enumerate(novel_terms, 1):
    ans = input(f"\n{i}. Term: {term}\nYour answer: ").strip()
    responses.append(ans)
end = time.time()

# Results
print("\nRESULTS")
correct = 0
for i, (term, user, corr) in enumerate(zip(novel_terms, responses, correct_answers), 1):
    mark = "✓" if user.lower() == corr.lower() else "✗"
    if mark == "✓":
        correct += 1
    print(f"{i}. {term} | Your: {user} | Correct: {corr} | {mark}")

score = correct / len(novel_terms) * 100
print(f"\nFinal Score: {correct}/{len(novel_terms)} ({score:.1f}%)")

# Save
timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
result = {
    "examples": examples,
    "novel_terms": novel_terms,
    "correct_answers": correct_answers,
    "user_responses": responses,
    "score": score,
    "total_time": end - start,
    "timestamp": timestamp
}

with open(f"response_human_{timestamp}.json", "w") as f:
    json.dump(result, f, indent=2)

np.savez(f"response_human_{timestamp}.npz", responses=np.array(responses), score=score)

print("\nSaved results.")

