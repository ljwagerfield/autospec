# AutoSpec

Test Framework for REST APIs that uses "rules" instead of "tests".

**Benefits:**

-   Rules are faster to write than tests.

-   Rules provide greater test coverage (as the framework exhausts test paths for you).

-   Rules make refactoring easier (change one rule instead of many tests).

## How it works

1.  Create a specification file for your REST API (we have [SDKs](#todo)).

2.  Run the AutoSpec docker image and pass in your specification file (we have [build tool plugins](#todo)).

3.  AutoSpec crawls your API and flags any bugs.

## Getting started with the examples

AutoSpec comes with a simple example to get you started:

1.  Example REST API (can be written in anything - e.g. could be a separate Python process):

    [`autospec.demo.RestApi`](demo/src/main/scala/autospec/demo/RestApi.scala)

2.  Example schema:

    [`autospec.demo.RestApiSchema`](demo/src/main/scala/autospec/demo/RestApiSchema.scala)

3.  Run a hardcoded test path:

    ```
    sbt 'demo/runMain autospec.demo.RunTestPlan'
    ```

4.  Run the crawler:

    ```
    sbt 'demo/runMain autospec.demo.RunGenerator'
    ```

## Technical points of interest

-   Endpoints are either "mutating" or "pure":

    -   Mutating is assumed if the endpoint has postconditions that reference other endpoints.

    -   Pure can be forced on an endpoint through the `forcePure` flag.

-   If an endpoint references another endpoint in its postconditions, that condition becomes "deferred" and will be checked if/when the referenced endpoint ever gets called with the same parameters specified in the postcondition.

-   A call to any mutating endpoint will clear all "deferred postconditions".

-   Workflows are discovered at runtime, rather than being calculated from a static analysis of the pre/postconditions.

    -   The problem is ensuring you traverse the entire tree of possible workflows.

    -   The additional problem is knowing what the entire tree looks like, so that you know whether or not you have fully traversed it.

    -   Our approach is stochastic: we check which endpoints' predicates pass at runtime, and randomly select the next endpoint to call from the list of available endpoints, giving more weight to endpoints that are less-frequently available.

    -   Granting more weight to less-frequently-available endpoints reduces the time taken to explore the entire tree (vs. a purely random selection).

    -   Over time, if we observe no new workflows, we determine that it's likely we've traversed the entire tree.
