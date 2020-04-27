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

## Where to start

1.  Read "points of interest" from the main README: this covers some high-level concepts.

2.  The `RequestGenerator` and `ResponseValidator` are the heart and soul of AutoSpec, so are a
    good place to start: everything else is plumbing.

## Code style

### Error handling

> TL;DR: return `EitherT` if a caller up the stack branches on the `Left`, or a failed monad if not.

Please follow this convention for error handling:

1.   Use `EitherT` when the caller can handle the error (i.e. is able to logically branch and
     perform some alternative action).

2.   Use `MonadError` when the caller does not know how to handle the error.

3.   If you consume an `EitherT` but neither you nor your caller will be branching on the error,
     convert the error to a `MonadError` and return an `F`.

     This flags "we encountered an error at this point (stack trace) that we are unable to handle".
     This typically indicates a bug in the code that needs fixing (hint: the fix is not to just
     bubble the error up to the caller by returning an `EitherT` instead... unless the caller can
     handle it!).

Advantages (v.s. propagating `EitherT` across the codebase):

-   Simpler method signatures.

-   More precise `A` types on `EitherT`.

-   More obvious to the caller when they should be handling errors (because the default stance isn't
    "just bubble it up" but is instead "stop and think about this error").

-   `MonadError` is already a great way of bubbling un-handle-able errors.

-   The original mantra behind 'errors in types' was about forcing you to think about the errors
    returned by a function, but if the error is `BaseError` then it could have come from the
    function you're calling, or from 4 levels deep, by which point you're back to square one of
    knowing nothing about the error again: you're not much better off than using exceptions. So
    please, try and use `EitherT` only when a caller actually performs some conditional logic
    based on that error, and if not, stick it into `MonadError` instead!

### Testing

Please write tests that are resilient to changes in implementation.

This means:

-   Testing more at an acceptance level.

This allows:

-   Faster refactors.

-   Safer refactors:

    Existing test suites can verify the correctness of new implementations.

    By contrast, if tests are too unit-level, they will need to be deleted and rewritten, carrying
    the risk of important test scenarios being removed.
