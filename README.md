# TestBot

Property-based testing for REST APIs.

Written in Scala 2.13.

The plan is to dockerise the service and create a specification language (i.e. `testbot.json`) that can be generated by REST APIs written in any language, and fed to a TestBot docker container for testing.

## Demo

1.  Example REST API (can be written in anything - e.g. could be a separate Python process):

    [`testbot.demo.RestApi`](demo/src/main/scala/testbot/demo/RestApi.scala)

2.  Example schema:

    [`testbot.demo.RestApiSchema`](demo/src/main/scala/testbot/demo/RestApiSchema.scala)

Use TestBot to crawl & test the REST API:

    sbt 'demo/runMain testbot.demo.RunGenerator'

Use TestBot to test a specific path through the REST API:

    sbt 'demo/runMain testbot.demo.RunTestPlan'

## Points of interest

-   Endpoints are either "mutating" or "pure":

    -   Mutating is assumed if the endpoint has postconditions that reference other endpoints.

    -   Pure can be forced on an endpoint through the `forcePure` flag.

-   If an endpoint references another endpoint in its postconditions, that condition becomes "deferred" and will be checked if/when the referenced endpoint ever gets called with the same parameters specified in the postcondition.

-   A call to any mutating endpoint will clear all "deferred postconditions".
