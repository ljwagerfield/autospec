# Contributor Guidelines

If you make a change to the `build.sbt` then you must run:

```bash
sbt githubWorkflowGenerate
```

This updates the GitHub Action that compiles the project in GitHub: it keeps things like the SBT
version, Java Version and Scala Version synced between the `build.sbt` and the GitHub Action.
