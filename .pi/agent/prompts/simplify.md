---
description:
  Simplify code (KISS, DRY, SRP, less state) in a target scope
argument-hint: "[last commit | staged | <path/to/file>]"
---

Target: ${@:-the code touched in this session}.

Resolve the target like this:

- empty: the code touched in this session (infer it from this
  conversation: the files edited and the work discussed).
- "last commit": the changes introduced by HEAD (look at `git show HEAD`
  / `git diff HEAD~1..HEAD`).
- "staged": the currently staged changes (`git diff --cached`).
- a commit, branch, or file/directory path: that commit/branch's diff,
  or that path.

Go over the target and simplify: KISS, DRY, SRP, minimize bloat, remove
unnecessary state (variables, fields, arguments), protect from state
explosion. Use steelman to argue each change. All this without omitting
critical functionality. Verify no breakage when done (tests / typecheck
/ compile, whatever the repo has).
