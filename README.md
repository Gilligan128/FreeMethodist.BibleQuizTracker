# FreeMethodist.BibleQuizTracker
Bible Quiz tournament and score tracker. This application can let quizmasters and quizzers quiz from wherever they are!
Uses the rules for Free Methodist Bible Quizzing.

# Getting Started

## Prerequisites
Install .Net 6 SDK.

# Tech Stack
The tech stack is based around functional programming paradigms and the [Model-View-Update (MVU)](https://guide.elm-lang.org/architecture/) architecture.

## F# 
This project is written in F# in order to minimize errors and boilerplate code.

## Blazor 
This project uses the Blazor framework (serverside) in order to write.

### Why serverside Blazor?
There are a few reasons why we chose serverside Blazor instead of Clientside WebAssembly:
1. we don't need to scale heavily
2. We don't anticipate a lot of heavy load needing to be offset onto the client.
3. we can directly invoke our business logic, skipping the whole api web backend "middleman"
4. It is trivial to change to clientside if we need the scaling.

## Bolero
This project uses the Bolero framework (serverside) in order to make easier writing Blazor in F#, as well as to leverage MVU.

# Roadmap
This is both the functional and technical roadmap


# Deployment
The application is deployed to this [Azure Subscription](https://portal.azure.com/#@gilligan128gmail.onmicrosoft.com/resource/subscriptions/57d74b02-3296-4a96-b65f-ae75cc2d7382/overview)

## Automation
We intend to automate:
- app deployment
- developer work environment provisioning
- production provisioning.
- data migrations/versioning

# Architecture
Some links, to  be organized later:

Testing and dependency management: https://fsharpforfunandprofit.com/posts/dependencies/

Following the Workflows - Pipelines pattern here: https://www.amazon.com/Domain-Modeling-Made-Functional-Domain-Driven/dp/1680502549