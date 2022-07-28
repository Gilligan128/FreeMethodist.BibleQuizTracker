module FreeMethodist.BibleQuizTracker.Server.CreateQuiz.Workflow

open FreeMethodist.BibleQuizTracker.Server.Workflow

module CreateQuiz =
    type Command = UnvalidatedQuiz
    type Error = | DbError of DbError
                 | RemoteError of string
                 | CodeAlreadyExists of string
                 | IndividualCompetitionStyle 
    type Event = | QuizCreated of QuizCode
    type Workflow = Command -> AsyncResult<Event list, Error>             