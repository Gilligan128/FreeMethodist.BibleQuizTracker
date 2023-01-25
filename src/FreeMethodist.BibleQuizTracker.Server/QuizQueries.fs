module FreeMethodist.BibleQuizTracker.Server.QuizQueries

open Azure.Storage.Blobs
open FreeMethodist.BibleQuizTracker.Server.Common.Pipeline

let private trimQuizPrefixes (name: string) =
    name.Replace("quizzes/", "").Replace("quiz-", "")



let getRecentCompletedQuizzes (blobServiceClient: BlobServiceClient) (tenantName: string) =
    asyncResult {
        let blobContainerClient =
            blobServiceClient.GetBlobContainerClient(tenantName.ToLower())

        let state = nameof Quiz.Completed
        let expression = $"\"State\" = '{state}'"

        if blobContainerClient.Exists().Value then

            let results =
                blobContainerClient.FindBlobsByTags(expression)
                |> Seq.map (fun item -> item.BlobName)
                |> Seq.map trimQuizPrefixes
                |> Seq.toList

            return results
        else
            return []
    }
