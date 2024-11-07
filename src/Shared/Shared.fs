namespace Shared

type Information = {
    skills: string
}

type IInformationApi = {
    getInformations: unit -> Async<Information list>
    addInformation: string -> Async<unit>
    deleteInformation: string -> Async<unit>
    updateInformation: string * string -> Async<unit>
}