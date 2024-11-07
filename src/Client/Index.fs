module Index

open Elmish
open SAFE
open Shared
open Feliz

type Model = {
    Informations: RemoteData<Information list>

    NewSkill: string
    IsInsertInputVisible: bool
    InsertErrorMessage: string option
    
    DeleteSkill: string
    IsDeleteInputVisible: bool
    DeleteErrorMessage: string option

    OldSkill: string
    UpdatedSkill: string
    IsUpdateInputVisible: bool
    UpdateErrorMessage: string option
}

type Msg =
    | LoadInformations of ApiCall<unit, Information list>

    | SetNewSkill of string
    | AddInformation
    | InformationAdded of Result<unit, string>
    | ShowInputSection
    | HideInputSection

    | SetDeleteSkill of string
    | DeleteInformation
    | InformationDeleted of Result<unit, string>
    | ShowDeleteInputSection
    | HideDeleteInputSection

    | SetOldSkill of string
    | SetUpdatedSkill of string
    | UpdateInformation
    | InformationUpdated of Result<unit, string>
    | ShowUpdateInputSection
    | HideUpdateInputSection



let informationsApi = Api.makeProxy<IInformationApi> ()

let init () =
    let initialModel = {
        Informations = NotStarted

        NewSkill = ""
        IsInsertInputVisible = false
        InsertErrorMessage = None
        
        DeleteSkill = ""
        IsDeleteInputVisible = false
        DeleteErrorMessage = None

        OldSkill = ""
        UpdatedSkill = ""
        IsUpdateInputVisible = false
        UpdateErrorMessage = None
    }

    let initialCmd = Cmd.ofMsg (LoadInformations(Start()))
    initialModel, initialCmd


let update msg model =
    match msg with
    | LoadInformations apiCallMsg ->
        match apiCallMsg with
        | Start() ->
            let loadCmd =
                Cmd.OfAsync.perform informationsApi.getInformations () (Finished >> LoadInformations)

            { model with Informations = Loading }, loadCmd
        | Finished informations ->
            {
                model with
                    Informations = Loaded informations
            },
            Cmd.none

    | ShowInputSection -> 
        { 
            model with 
                IsInsertInputVisible = true 
        }, Cmd.none

    | HideInputSection -> 
        { 
            model with 
                IsInsertInputVisible = false 
        }, Cmd.none

    | SetNewSkill newSkill -> 
        { 
            model with 
                NewSkill = newSkill 
        }, Cmd.none

    | AddInformation ->
        if model.NewSkill.Trim() = "" then
            let errorMessage = "Please fill in Skills fields."

            {
                model with
                    InsertErrorMessage = Some errorMessage
            },
            Cmd.none
        else
            let addCmd =
                Cmd.OfAsync.either
                    informationsApi.addInformation
                    (model.NewSkill)
                    (fun _ -> InformationAdded(Ok()))
                    (fun ex -> InformationAdded(Error ex.Message))

            {
                model with
                    IsInsertInputVisible = false
                    InsertErrorMessage = None
            },
            addCmd

    | InformationAdded result ->
        match result with
        | Ok() ->
            let loadCmd = LoadInformations(Start()) |> Cmd.ofMsg

            {
                model with
                    Informations = Loading
                    NewSkill = ""
            },
            loadCmd
        | Error errMsg ->
            printfn "Error adding information: %s" errMsg
            { model with Informations = Loading }, Cmd.none

    | ShowDeleteInputSection ->
        {
            model with
                IsDeleteInputVisible = true
        },
        Cmd.none

    | HideDeleteInputSection ->
        {
            model with
                IsDeleteInputVisible = false
                DeleteSkill = ""
        },
        Cmd.none

    | SetDeleteSkill deleteSkill -> 
        { 
            model with 
                DeleteSkill = deleteSkill 
        }, Cmd.none

    | DeleteInformation ->
        if model.DeleteSkill.Trim() = "" then
            let errorMessage = "Please enter the skill you want to delete."

            {
                model with
                    DeleteErrorMessage = Some errorMessage
            },
            Cmd.none
        else
            let deleteCmd =
                Cmd.OfAsync.either
                    informationsApi.deleteInformation
                    model.DeleteSkill
                    (fun _ -> InformationDeleted(Ok()))
                    (fun ex -> InformationDeleted(Error ex.Message))

            {
                model with
                    IsDeleteInputVisible = false
                    DeleteErrorMessage = None
            },
            deleteCmd

    | InformationDeleted result ->
        match result with
        | Ok() ->
            let loadCmd = LoadInformations(Start()) |> Cmd.ofMsg

            {
                model with
                    Informations = Loading
                    DeleteSkill = ""
            },
            loadCmd
        | Error errMsg ->
            printfn "Error deleting information: %s" errMsg
            { model with Informations = Loading }, Cmd.none
    | ShowUpdateInputSection ->
        {
            model with
                IsUpdateInputVisible = true
        },
        Cmd.none

    | HideUpdateInputSection ->
        {
            model with
                IsUpdateInputVisible = false
                OldSkill = ""
                UpdatedSkill = ""
        },
        Cmd.none

    | SetOldSkill oldSkill -> 
        { 
            model with 
                OldSkill = oldSkill 
        }, Cmd.none

    | SetUpdatedSkill updatedSkill ->
        {
            model with
                UpdatedSkill = updatedSkill
        },
        Cmd.none

    | UpdateInformation ->
        if model.OldSkill.Trim() = "" || model.UpdatedSkill.Trim() = "" then
            let errorMessage = "Please enter both the skill to update and the new skill name."

            {
                model with
                    UpdateErrorMessage = Some errorMessage
            },
            Cmd.none
        else
            let updateCmd =
                Cmd.OfAsync.either
                    informationsApi.updateInformation
                    (model.OldSkill, model.UpdatedSkill)
                    (fun _ -> InformationUpdated(Ok()))
                    (fun ex -> InformationUpdated(Error ex.Message))

            {
                model with
                    IsUpdateInputVisible = false
                    UpdateErrorMessage = None
            },
            updateCmd

    | InformationUpdated result ->
        match result with
        | Ok() ->
            let loadCmd = LoadInformations(Start()) |> Cmd.ofMsg

            {
                model with
                    Informations = Loading
                    OldSkill = ""
                    UpdatedSkill = ""
            },
            loadCmd
        | Error errMsg ->
            printfn "Error updating information: %s" errMsg
            { model with Informations = Loading }, Cmd.none

module ViewComponents =
    let navBar =
        Html.nav [
            prop.className "flex justify-end fixed top-0 left-0 w-screen bg-rose-700 z-10 shadow-md p-4"
            prop.children [
                Html.ul [
                    prop.className "flex space-x-4 mr-8"
                    prop.children [
                        Html.li [
                            Html.a [
                                prop.href "#skills"
                                prop.className "text-white text-xl p-4 hover:bg-gray-300"
                                prop.text "Skills"
                            ]
                        ]
                        Html.li [
                            Html.a [
                                prop.href "#welcome"
                                prop.className "text-white text-xl p-4 hover:bg-gray-300"
                                prop.text "Go To Top"
                            ]
                        ]
                    ]
                ]
            ]
        ]

    let welcomeSection =
        Html.div [
            prop.id "welcome"
            prop.className
                "flex flex-col items-center justify-center h-screen bg-gradient-to-r from-gray-800 via-gray-900 to-black text-center"
            prop.children [
                Html.h1 [
                    prop.className "text-8xl font-bold text-white mb-5"
                    prop.text "Welcome everyone"
                ]
                Html.p [
                    prop.className "text-4xl italic mt-2 text-rose-700"
                    prop.text "This website is my resume."
                ]
            ]
        ]

    let insertSection model dispatch =
        Html.div [
            match model.InsertErrorMessage with
            | Some message -> Html.p [ prop.className "text-rose-700 mb-2"; prop.text message ]
            | None -> Html.none

            Html.input [
                prop.value model.NewSkill
                prop.onChange (SetNewSkill >> dispatch)
                prop.placeholder "Enter new skill"
                prop.className "border p-2 mb-4"
            ]
            Html.div [
                prop.className "flex space-x-2"
                prop.children [
                    Html.button [
                        prop.className "px-6 py-2 bg-gray-800 text-rose-700 font-bold rounded-md hover:bg-gray-700"
                        prop.text "Add"
                        prop.onClick (fun _ -> dispatch AddInformation)
                    ]
                    Html.button [
                        prop.className "px-6 py-2 bg-gray-800 text-rose-700 font-bold rounded-md hover:bg-gray-700"
                        prop.text "Cancel"
                        prop.onClick (fun _ -> dispatch HideInputSection)
                    ]
                ]
            ]
        ]

    let deleteSection model dispatch =
        Html.div [
            match model.DeleteErrorMessage with
            | Some message -> Html.p [ prop.className "text-rose-700 mb-2"; prop.text message ]
            | None -> Html.none

            Html.input [
                prop.value model.DeleteSkill
                prop.onChange (SetDeleteSkill >> dispatch)
                prop.placeholder "Enter skill to delete"
                prop.className "border p-2 mb-4"
            ]
            Html.div [
                prop.className "flex space-x-2"
                prop.children [
                    Html.button [
                        prop.className "px-6 py-2 bg-gray-800 text-rose-700 font-bold rounded-md hover:bg-gray-700"
                        prop.text "Delete"
                        prop.onClick (fun _ -> dispatch DeleteInformation)
                    ]
                    Html.button [
                        prop.className "px-6 py-2 bg-gray-800 text-rose-700 font-bold rounded-md hover:bg-gray-700"
                        prop.text "Cancel"
                        prop.onClick (fun _ -> dispatch HideDeleteInputSection)
                    ]
                ]
            ]
        ]

    let updateSection model dispatch =
        Html.div [
            match model.UpdateErrorMessage with
            | Some message -> Html.p [ prop.className "text-rose-700 mb-2"; prop.text message ]
            | None -> Html.none

            Html.div [
                prop.className "flex space-x-2"
                prop.children [
                    Html.input [
                        prop.value model.OldSkill
                        prop.onChange (SetOldSkill >> dispatch)
                        prop.placeholder "Enter skill to update"
                        prop.className "border p-2 mb-2"
                    ]
                    Html.input [
                        prop.value model.UpdatedSkill
                        prop.onChange (SetUpdatedSkill >> dispatch)
                        prop.placeholder "Enter new skill name"
                        prop.className "border p-2 mb-2"
                    ]
                ]
            ]
            Html.div [
                prop.className "flex space-x-2 justify-center"
                prop.children [
                    Html.button [
                        prop.className "px-6 py-2 bg-gray-800 text-rose-700 font-bold rounded-md hover:bg-gray-700"
                        prop.text "Update"
                        prop.onClick (fun _ -> dispatch UpdateInformation)
                    ]
                    Html.button [
                        prop.className "px-6 py-2 bg-gray-800 text-rose-700 font-bold rounded-md hover:bg-gray-700"
                        prop.text "Cancel"
                        prop.onClick (fun _ -> dispatch HideUpdateInputSection)
                    ]
                ]
            ]
        ]

    let skillsSection model dispatch =
        Html.div [
            prop.id "skills"
            prop.className
                "flex items-center justify-center h-screen w-full bg-gradient-to-r from-gray-800 via-gray-900 to-black"
            prop.children [
                Html.div [
                    prop.className "flex flex-row max-w-5xl items-center space-x-40"
                    prop.children [
                        Html.div [
                            prop.className "flex flex-col space-y-4"
                            prop.children [
                                Html.h1 [
                                    prop.className "flex justify-center text-4xl font-bold text-rose-700"
                                    prop.text "< My Skills />"
                                ]
                                Html.div [
                                    prop.className
                                        "grid grid-cols-1 md:grid-cols-2 gap-4 text-xl italic mt-4 text-white"
                                    prop.children (
                                        match model.Informations with
                                        | Loaded informations ->
                                            informations
                                            |> List.mapi (fun index info ->
                                                Html.div [
                                                    prop.key index
                                                    prop.className "flex justify-evenly"
                                                    prop.text $"{info.skills}"
                                                ])
                                        | Loading -> [ Html.p "Loading..." ]
                                        | NotStarted -> [ Html.p "No data loaded." ]
                                    )
                                ]


                                if model.IsInsertInputVisible then
                                    insertSection model dispatch

                                if model.IsDeleteInputVisible then
                                    deleteSection model dispatch

                                if model.IsUpdateInputVisible then
                                    updateSection model dispatch

                                if
                                    not model.IsInsertInputVisible
                                    && not model.IsDeleteInputVisible
                                    && not model.IsUpdateInputVisible
                                then
                                    Html.button [
                                        prop.className
                                            "px-6 py-2 bg-gray-800 text-rose-700 font-bold rounded-md hover:bg-gray-700"
                                        prop.text "Add"
                                        prop.onClick (fun _ -> dispatch ShowInputSection)
                                    ]

                                if
                                    not model.IsDeleteInputVisible
                                    && not model.IsInsertInputVisible
                                    && not model.IsUpdateInputVisible
                                then
                                    Html.button [
                                        prop.className
                                            "px-6 py-2 bg-gray-800 text-rose-700 font-bold rounded-md hover:bg-gray-700"
                                        prop.text "Delete"
                                        prop.onClick (fun _ -> dispatch ShowDeleteInputSection)
                                    ]

                                if
                                    not model.IsUpdateInputVisible
                                    && not model.IsDeleteInputVisible
                                    && not model.IsInsertInputVisible
                                then
                                    Html.button [
                                        prop.className
                                            "px-6 py-2 bg-gray-800 text-rose-700 font-bold rounded-md hover:bg-gray-700"
                                        prop.text "Update"
                                        prop.onClick (fun _ -> dispatch ShowUpdateInputSection)
                                    ]
                            ]
                        ]
                        Html.div [
                            Html.img [
                                prop.src "https://i.ibb.co/vh0hXhD/image.jpg"
                                prop.className "w-96 h-auto rounded-lg shadow-lg"
                                prop.alt "Profile Picture"
                            ]
                        ]
                    ]
                ]
            ]
        ]


let view model dispatch =
    Html.section [
        ViewComponents.navBar
        ViewComponents.welcomeSection
        ViewComponents.skillsSection model dispatch
    ]