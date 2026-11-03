---
description:
  Review a target for robustness and long-term maintainability, fix
  issues in-place
argument-hint: "[last commit | staged | <path/to/file>]"
---

Target: ${@:-the changes touched in this session}.

Resolve the target like this:

- empty: the changes touched in this session (infer them from this
  conversation: the files edited and the work discussed).
- "last commit": the changes introduced by HEAD (`git show HEAD` /
  `git diff HEAD~1..HEAD`).
- "staged": the currently staged changes (`git diff --cached`).
- a commit, branch, or file/directory path: that commit/branch's diff,
  or that path.

Review the target for being scalable, rigid, easy to maintain, easy to
fix, easy to add features, simple to reason about and understand, easy
to fix at 2am in a year. I want this part of the code to be the most
robust part of our code. Eliminate classes of bugs with a better design,
not just squashing some bug. Aim for single source of truth. Fix all the
issues & concerns (if there are any). If a fix would be too large or
risky to do well, flag it clearly instead of forcing it.
