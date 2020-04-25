# Contributor Guidelines

## IntelliJ Setup

-   Use `scalafmt` formatter when IntelliJ prompts.

-   Use IntelliJ's built-in support (do not install the `scalafmt` plugin).

-   Enable 'format on save': `Editor > Code Style > Scala > Format On Save > ENABLE`

## Making changes to `build.sbt`

If you make a change to the `build.sbt` then you must run:

```bash
sbt githubWorkflowGenerate
```

This updates the GitHub Action that compiles the project in GitHub: it keeps things like the SBT
version, Java Version and Scala Version synced between the `build.sbt` and the GitHub Action.
