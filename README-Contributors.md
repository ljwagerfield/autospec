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

### Error Handling

-   Exceptions: we `throw` when the calling code cannot recover from the failure in a "non-generic way" (†) _and_ there's no specific error message to return to the end-user.

-   Errors: we return `Left` when the calling code can recover from the failure in a "non-generic way" (†) _or_ there's a specific message we want to return to the end-user.

(†) "non-generic way" means handling the error in a way that isn't simply `.recover { case NonFatal(e) => ... generic recovery ... }`

#### Why use both approaches?

Many developers believe `Left` replaces `throw`, but to get the most value out of `Left`, you
should really `throw` your unhandleable errors. Here's why:

-   Durable code already handles exceptions (i.e. all well-written servers will translate an
    unhandled `Exception` to a `500`). So whether you're a `Left` zealot or not, you will still
    be handling / supporting exceptions at the root of your application, and also in the parts where
    a cleanup must occur on error (e.g. closing a database connection).

-   Thus, we agree that well-written code must be prepared for the possibility of a generic
    exception both at the root of the application, and anywhere that cleanup must occur.

-   So, let's use `throw` for unhandleable errors.

-   Then, if `Either[L, _]` is left for handle-able errors only:

    -   We alleviate the `BaseError` problem (as we're bubbling unhandleable errors invisibly now).

    -   Forces you to think whether a `Left` from a sub-call should be bubbled (i.e. is handleable by your caller) or whether it
        is unexpected in this context, in which case a bug has occured, in which case you should convert to an `Exception` and throw.

    -   `Either[L, _]` becomes more descriptive about the preconditions of the method.

#### No `BaseError` types

-   We do not include a `BaseError` type in the codebase.

-   Common error functionality (like HTTP status codes, etc.) are handled through type classes instead, rather than extending a common error type.

-   This prevents the lowest common denominator error problem: a dangerous anti-pattern that encourages methods to bubble-up `BaseError` types without stopping to think about why they are occurring. This often results in methods like `def deleteUser(id: UserId)` returning nonsensical left-values like `PaymentExpired`, which are hard to track down, and may represent bugs that should be `thrown` instead (see above).

    -   Yes: this does come at the cost of writing more error types!

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
