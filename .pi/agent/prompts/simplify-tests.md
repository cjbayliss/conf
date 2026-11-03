---
description: Simplify and consolidate tests in a target scope
argument-hint: "[last commit | staged | <path/to/test-file>]"
---

Target: ${@:-the tests touched in this session}.

Resolve the target like this:

- empty: the tests touched in this session (infer them from this
  conversation: the files edited and the work discussed).
- "last commit": the test changes introduced by HEAD (`git show HEAD` /
  `git diff HEAD~1..HEAD`).
- "staged": the currently staged changes (`git diff --cached`).
- a commit, branch, or file/directory path: that commit/branch's diff,
  or that path.

Go over the tests in the target and simplify: consolidate tests, remove
bullshit tests, make them less prone to break due to code /
implementation changes / timing issues (sleeps on slow machines). Use
steelman to argue each change. Run the affected tests when done to
confirm they still pass, and make sure the remaining suite still covers
the real behavior (not just fewer lines).
