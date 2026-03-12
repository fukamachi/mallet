# Rule Classification Guide

This document defines the criteria for classifying Mallet rules along two orthogonal axes: **category** (what kind of issue) and **severity** (how urgently to fix it).

Inspired by Clippy (Rust), RuboCop (Ruby), and SonarQube.

## Categories

Categories describe the *nature* of the issue. A rule belongs to exactly one category.

### `:correctness`

Code that is **objectively wrong** or defeats language semantics. No reasonable interpretation makes the code intentional.

- Criterion: The code will cause runtime errors, silently produce wrong results, or negate language guarantees
- False positive tolerance: Near zero
- Example: `ecase` with `otherwise` clause (defeats exhaustiveness checking)

### `:suspicious`

Code that is **most likely a bug**, but *might* be intentional. If the author can provide a good reason, it may be suppressed.

- Criterion: The pattern is almost never correct in practice; intentional use is the exception, not the rule
- Distinction from `:correctness`: There exists a conceivable (rare) intentional use
- Example: `eval` usage at runtime, runtime `intern`/`unintern`

### `:practice`

Code that **works but uses a widely discouraged pattern**. A better alternative exists with clear rationale.

- Criterion: The CL community broadly agrees this pattern should be avoided, and a concrete better alternative can be stated
- Distinction from `:suspicious`: Not likely a bug, just a bad habit
- Distinction from `:style`: Not about aesthetics or idiom; about robustness, maintainability, or safety
- Example: `ignore-errors` (swallows all conditions), `package::internal-symbol` access (breaks encapsulation)

### `:cleanliness`

**Dead code** or unused declarations. The code is not wrong but adds noise.

- Criterion: Something was declared/imported/defined but never referenced
- Example: Unused variables, unused local nicknames, unused imported symbols

### `:style`

**Idiomatic preferences** where multiple valid approaches exist. Rules here enforce the CL community's conventional way of writing things.

- Criterion: Both the flagged and suggested forms are correct, but one is more idiomatic
- Distinction from `:practice`: No robustness or safety concern; purely about expression
- Example: `if` without else (use `when`/`unless`), `+constant+` naming convention

### `:format`

**Textual formatting** detectable from raw text without parsing. Typically auto-fixable.

- Criterion: Concerns whitespace, line structure, or file-level text properties
- Example: Trailing whitespace, tab characters, closing parentheses on their own line

### `:metrics`

**Quantitative measurements** of code properties. Not a judgment of correctness or style.

- Criterion: A numeric measurement exceeds a configurable threshold
- Distinction from all others: Purely informational; the code may be perfectly fine
- Example: Function length, cyclomatic complexity

## Severity

Severity is orthogonal to category and determines CI behavior. Each rule has a default severity, which users can override per-rule via configuration.

### `:error`

The code is almost certainly broken. Reserved for `:correctness` rules with near-zero false positive rates.

- CI: Fails (exit 1 with `--fail-on warning` or `--fail-on error`)
- Expectation: Must be fixed

### `:warning`

The code is likely problematic and should be addressed. The default for most actionable rules.

- CI: Fails by default (exit 1 with `--fail-on warning`, the default)
- Expectation: Should be fixed; suppress with reason if intentional

### `:info`

Worth knowing about but not necessarily wrong. Measurements, optional preferences, and low-confidence suggestions.

- CI: Does not fail by default (exit 0 unless `--fail-on info`)
- Expectation: Fix at your discretion

## Relationship Between Severity and Default Preset

Severity and default inclusion are **independent decisions**.

- **Severity** answers: "When this rule is enabled, how serious is a violation?"
- **Default** answers: "Should this rule be enabled out of the box?"

A rule can be `:warning` severity (violations should be fixed) but excluded from default (not every project needs it). For example, `error-with-string-only` is `:warning` because untyped conditions are a real problem worth fixing — but not every project prioritizes structured error handling, so it is not in the default preset.

Conversely, a rule's severity should not be lowered just to exclude it from default. If a violation genuinely warrants attention when the rule is enabled, it should be `:warning` regardless of default inclusion.

## Default Preset

The `:default` preset enables rules that meet **all** of these criteria:

1. **Broad consensus** — The CL community widely agrees this is a real issue, not a preference
2. **Low false positive rate** — The rule rarely flags intentional code
3. **Actionable** — The violation message tells you what to fix

Rules excluded from `:default` typically fail on one of:

- **No consensus** — Reasonable developers disagree (e.g., line length limits, `+constant+` naming)
- **Noisy** — Too many false positives in real-world code without context (e.g., unused loop variables)
- **Niche** — Useful for specific project policies but not universally applicable (e.g., `error-with-string-only`)
