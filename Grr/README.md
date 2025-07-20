# GRR (Glob/Regex Resolver)

This library provides a super simple resolver for simple glob/regular expression texts.
It solves the two into an internal structure and check for matches from a list of supplied texts.
It does not do IO operations.
For now, it only returns exact matches. If I want to add the ability to do partial matches (part of the text matches the pattern), I'll put it in later.

## Information

### Exposed Types:

- Pattern = RegEx | Glob
- CaseSensitive = Bool

### Exposed Functions:

- Grr:
  - `match :: Pattern -> CaseSensitive -> Text -> Text -> Bool` checks if the pattern in the first Text is matched by the second Text.
  - `filter :: Pattern -> CaseSensitive -> Text -> [Text] -> [Text]` filters and keeps only the Texts that match the first text, which is a pattern.
  - `order :: Text -> [Text] -> [Text]` sorts the texts in the list of Texts by closeness to the first Text.
  - `score :: Text -> Text -> Int` provides the Levenshtein distance between the two provided Texts.

- Grr.Regex:
  - `regex :: Text -> Text -> Bool` matches the second Text to the first Text, which is a Regular Expression.

- Grr.Glob:
  - `glob :: Text -> Text -> Bool` matches the second Text to the first Text, which is a Glob pattern.
