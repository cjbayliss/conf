---
description:
  Verify no breakage in a target scope using 3 parallel subagents with
  different styles
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

Run verification of no breakage of the target. Start with the cheap
deterministic checks yourself (tests, typecheck, compile). Then run 3
smart parallel subagents, each with a completely different style of
prompt: one creative, one more scrutinizing, and think of the most
appropriate third one, so we'll have the best chance to find all the
issues. Report anything found before fixing.
