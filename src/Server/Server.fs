module Server

open SAFE
open Saturn
open Shared

open Npgsql.FSharp

let connectionString =
    "Host=localhost; Database=FunctionalDB; Username=postgres; Password=postgres;"

let getAllInformations (connectionString: string) : Information list =
    try
        printfn "Show skills"
        
        connectionString
        |> Sql.connect
        |> Sql.query "SELECT * FROM information"
        |> Sql.execute (fun read -> { skills = read.text "skills" })
    with ex ->
        printfn "Error occurred: %s" ex.Message
        raise ex

let addInformation (connectionString: string) (skills: string) : unit =
    try
        printfn "Adding new skills = %s" skills

        connectionString
        |> Sql.connect
        |> Sql.query "INSERT INTO information (skills) VALUES (@skills);"
        |> Sql.parameters [ "skills", Sql.string skills ]
        |> Sql.executeNonQuery
        |> ignore
    with ex ->
        printfn "Error occurred: %s" ex.Message
        raise ex

let deleteInformation (connectionString: string) (skills: string) : unit =
    try
        printfn "Deleting skill = %s" skills

        connectionString
        |> Sql.connect
        |> Sql.query "DELETE FROM information WHERE skills = @skills;"
        |> Sql.parameters [ "skills", Sql.string skills ]
        |> Sql.executeNonQuery
        |> ignore
    with ex ->
        printfn "Error occurred: %s" ex.Message
        raise ex

let updateInformation (connectionString: string) (oldSkill: string) (newSkill: string) : unit =
    try
        printfn "Updating skill from %s to %s" oldSkill newSkill

        connectionString
        |> Sql.connect
        |> Sql.query "UPDATE information SET skills = @newSkill WHERE skills = @oldSkill;"
        |> Sql.parameters [ "newSkill", Sql.string newSkill; "oldSkill", Sql.string oldSkill ]
        |> Sql.executeNonQuery
        |> ignore
    with ex ->
        printfn "Error occurred: %s" ex.Message
        raise ex

let informationsApi ctx = {
    getInformations =
        fun () -> async {
            let getInfo = getAllInformations connectionString
            return getInfo
        }
    addInformation =
        fun (skills) -> async {
            let addInfo = addInformation connectionString skills
            return addInfo
        }
    deleteInformation =
        fun (skills) -> async {
            deleteInformation connectionString skills
            return ()
        }
    updateInformation =
        fun (oldSkill, newSkill) -> async {
            updateInformation connectionString oldSkill newSkill
            return ()
        }
}

let webApp = Api.make informationsApi

let app = application {
    use_router webApp
    memory_cache
    use_static "public"
    use_gzip
}

[<EntryPoint>]
let main _ =
    run app
    0