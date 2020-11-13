[![](https://github.com/autospec/autospec/workflows/Continuous%20Integration/badge.svg)](https://github.com/autospec/autospec/actions?query=workflow%3A%22Continuous+Integration%22)

# AutoSpec

**Property-based acceptance testing for REST APIs.**

Benefits:

-   Properties are faster to write than tests.

-   Properties provide greater test coverage (as the framework exhausts test paths for you).

-   Properties make refactoring easier (change one property instead of many tests).

## How it works

1.  Create a specification file for your REST API.

2.  Run your REST API. 

3.  Run AutoSpec, passing it your specification file.

AutoSpec will traverse your API, calling its endpoints in all their possible sequences, and will
flag the bugs that it detects.

## AutoSpec is not just a fuzzer!

AutoSpec analyses the pre/post conditions on your API endpoints, and knows which API endpoints
are related, how they're related, and therefore what order to call them in.

In choosing endpoints to call, AutoSpec applies a greater weight to endpoints that haven't recently
been available (i.e. their preconditions haven't been met), as this implies these endpoints are
part of a deep workflow, so should be given priority.

This behaviour causes AutoSpec to statistically favour a happy path -- i.e. a path you'd expect a
user to follow -- but will occasionally take detours just as a normal API user would do, in order
to find edge-cases.

## Getting started with the examples

AutoSpec comes with a simple example to get you started:

1.  Example REST API (can be written in anything - e.g. could be a separate Python process):

    [`autospec.demo.RestApi`](demo/src/main/scala/autospec/demo/RestApi.scala)

2.  Example schema:

    [`autospec.demo.RestApiSchema`](demo/src/main/scala/autospec/demo/RestApiSchema.scala)

3.  Run the crawler:

    ```
    sbt 'demo/runMain autospec.demo.RunGenerator'
    ```

    You will see a flurry of requests, as AutoSpec indefinitely crawls the embedded REST API
    to find errors.

4.  Try introducing a bug in [`autospec.demo.RestApi`](demo/src/main/scala/autospec/demo/RestApi
.scala)!

## Technical points of interest

-   Endpoints are either "mutating" or "pure":

    -   Mutating is assumed if the endpoint has postconditions that reference other endpoints.

    -   Pure can be forced on an endpoint through the `forcePure` flag.

-   If an endpoint references another endpoint in its postconditions, that condition becomes
    "deferred" and will be checked if/when the referenced endpoint ever gets called with the same
    parameters specified in the postcondition.

    -   Subsequent calls to mutating endpoints will clear all "deferred postconditions".

-   Stochastic processes are used to discover paths through the application at runtime.

    -   The goal is to traverse every possible workflow (path) through the application (graph).

    -   The challenge is not knowing what the graph looks like. This makes exhaustively
        traversing it impossible, since you never know what, if anything, is left to traverse.

    -   "Dynamic connectivity" is the reason why it's impossible to know the shape of the graph:
        if each node represents an API endpoint, you cannot determine which nodes are visitable from
        a given node until you visit it -- and the next time you visit this node, a different set
        of nodes may be visitable. This is called "full dynamic connectivity" and makes graphs
        difficult to traverse! This behaviour exists because pre/post-conditions only express
        subsets of the input/output data, so you need the _entire_ application state at runtime to
        reliably evaluate preconditions (to know if an endpoint is callable), and since endpoints
        can potentially mutate, the set of visitable nodes you can infer now are only valid for
        one more traversal, and will need to be recalculated on traversing the next node.

    -   Static analysis of the application (i.e. through inspecting pre/post-conditions) is
        impossible for most real-world applications, since the conditions usually only express a
        subset of what's actually changing in the application's state, making it impossible
        to statically build a full picture of the application's state to identify which endpoints
        are callable from when: therefore, we must examine the application's state at runtime.

    -   Our approach is stochastic: we check which endpoints' predicates pass at runtime, and
        randomly select the next endpoint to call from the list of available endpoints, giving more
        weight to endpoints that are less-frequently available.

    -   Granting more weight to less-frequently-available endpoints reduces the time taken to
        explore the entire graph (vs. a purely random selection).

    -   Over time, if we observe no new workflows, we determine that it's likely we've traversed the
        entire graph.

-   Parameter generation is achieved by reverse-engineering endpoint preconditions:

    -   `foldLeft` is used as a lowest-common-denominator for all collection manipulations
         (e.g. `.min`, `.count`, `.map`, `.reverse`, `.flatMap`, etc. are converted to `.foldLeft`).

    -   Turing Machines are then used as an 'encoding' for the predicates (i.e. after converting the
        predicates to `foldLeft`s, the `foldLeft`s are then transpiled to Turing Machines).

        -   Turing Machines are used over FSMs or PDAs since they can represent any sequence
            expressible by `foldLeft`s.

        -   The resulting Turing Machine is then "run backwards" from end to start to generate
            valid input sequences that abide the preconditions of the endpoint.
