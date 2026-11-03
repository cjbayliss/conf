---
description: Review a plan file for robust design, fix issues in-place
argument-hint: "[<path/to/plan-file>]"
---

Target: ${@:-the plan file touched in this session}. If the target is a
path, use that file.

Review the plan for being scalable, rigid, easy to maintain, easy to
fix, easy to add features, simple to reason about and understand, easy
to fix at 2am in a year. I want this part of the code to be the most
robust part of our code. Eliminate classes of bugs with a better design,
not just squashing some bug. Aim for single source of truth. Fix & edit
all the issues & concerns in the plan file in-place (if there are any).
If a fix would be too large or make the plan worse, flag it instead of
forcing it.
