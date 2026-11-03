---
description: Refactor spaghetti code into robust, maintainable design
argument-hint: "<path/to/file-or-module> [extra context]"
---

Target: ${@:-the code touched in this session}. If the target is a path,
use that file or module.

The target is spaghetti. Refactor it so it will be scalable, rigid, easy
to maintain, easy to fix, easy to add features, simple to reason about
and understand, easy to fix at 2am in a year. I want this part of the
code to be the most robust part of our code. Eliminate classes of bugs
with a better design. Preserve all existing behavior unless explicitly
told otherwise. Verify no breakage when done (tests / typecheck /
compile, whatever the repo has).
