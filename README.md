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
This is both the functional and technical roadmap.

## Functional Roadmap
Overall theme here is "do what it takes to get accurate, visible, and persistent scores, then do the rest"
- [x] Quizmaster can track and record all pertinent scoring info for a quiz. 
  - includes adding Quizzers to a quiz.
  - does not include Quizzer being able to "jump"
- [ ] Tournament Directors, and Regional directors can collect scores after a quiz is done
- [ ] Spectators can see live scores for a quiz.
- [ ] Quizzers can participate and "jump" in quizzes.
- [ ] Quizmasters can see the history of a quiz, and possibly make adjustments.
- [ ] Quizzers can track "fun" quizzes, scoring how they see fit and not being limited in jumpers.

## Technical Roadmap
This is more about the technical challenges we are likely to encounter, and setting up architecture to solve them.
- [X] Anyone who is viewing a running quiz, spectating score can see live updates of that quiz.
  - This is solved by having a domain-specific SignalR connection (different from Blazor's builtin one) and publishing domain events through it.
- [ ] Quizzers can jump at roughly the same time without worrying about getting "bad" jump orders
  - I intend to solve this by making "jumps" a separate blob container from quizzes so that jumps don't override each other.
- [ ] Differing latencies among quizzers won't affect jump order
  - This requires either clock syncing between server-client or collecting latency of each user and using that in jump order calculations.
# Deployment
Application url: https://freemethodist-bible-quizzing.azurewebsites.net

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

We are also following the "external message" pattern to dispatch updates to parent components, guided by [this article](https://medium.com/@MangelMaxime/my-tips-for-working-with-elmish-ab8d193d52fd).

# Project Structure
F# only supports unidirectional dependencies, in file location order. 

## All dependencies point inward.
Practicing Domain-driven Design, all pipelines depend on Workflow files, which are either interfaces or public types.
For each workflow, there is likely a [workflow name].Workflow.fs followed by a [workflow name].pipelines.fs.

Example: 'Common.Pages.fs' can depend on 'Common.pipeline.fs', which in turn can depend on 'Common.Workflow.fs' but not vice versa.

## Common stuff 
Anything labeled "common" depends on nothing else within its 'circle' of the code and should be the first file of its section.
Circles meaning: "Workflow" and "Pipeline" for backend, "Pages/Components" for frontend. 

Example: 'Common.Pipeline.fs' cannot depend upon 'OverrideTeamScore.Pipeline.fs' but CAN depend on 'OverrideTeamScore.Worfklow.fs'

## Main stuff
In the reverse of "common", "main" files can depend on any other file of its circle but nothing else in that layer should depend on it.

Example: 'Main.Pipeline.fs' can depend on 'OverrideTeamScore.pipeline.fs' but not vice versa. It CAN depend on 'Main.Workflow.fs' as "all dependencies point inward"

