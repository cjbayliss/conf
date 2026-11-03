---
description:
  Clean up comments (remove trivial, keep WHY not WHAT) in a target
  scope
argument-hint: "[last commit | staged | <path/to/file>]"
---

Target: ${@:-the comments touched in this session}.

Resolve the target like this:

- empty: the comments touched in this session (infer them from this
  conversation: the files edited and the work discussed).
- "last commit": the changes introduced by HEAD (`git show HEAD` /
  `git diff HEAD~1..HEAD`).
- "staged": the currently staged changes (`git diff --cached`).
- a commit, branch, or file/directory path: that commit/branch's diff,
  or that path.

Remove trivial comments in the target. Also modify the comments you do
keep to explain concisely WHY on non obvious stuff, not WHAT. Remove
bloat (every comment paragraph should be justified). Tone and language:
down to earth, warm, concise (without omitting the non obvious / novel /
interesting details), simple and easy to read even for non english
natives, tell it in a story! No em-dashes, do not show it was written by
AI.
