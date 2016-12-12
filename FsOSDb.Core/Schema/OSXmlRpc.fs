namespace FsOSBd.Core

module OSXmlRpc = 
    open CookComputing.XmlRpc
    
    [<XmlRpcUrl("http://api.opensubtitles.org/xml-rpc")>]
    type IStateName = 
        inherit IXmlRpcProxy
        
        [<XmlRpcMethod("LogIn")>]
        abstract LogIn : string -> string -> string -> string -> XmlRpcStruct
        
        [<XmlRpcMethod("LogOut")>]
        abstract LogOut : string -> XmlRpcStruct
        
        [<XmlRpcMethod("NoOperation")>]
        abstract NoOperation : string -> XmlRpcStruct
        
        [<XmlRpcMethod("SearchSubtitles")>]
        abstract SearchSubtitles : string -> XmlRpcStruct [] -> XmlRpcStruct
        
        [<XmlRpcMethod("SearchMoviesOnIMDB")>]
        abstract SearchMoviesOnIMDB : string -> string -> XmlRpcStruct
        
        [<XmlRpcMethod("DownloadSubtitles")>]
        abstract DownloadSubtitles : string -> string [] -> XmlRpcStruct
        
        [<XmlRpcMethod("CheckMovieHash")>]
        abstract CheckMovieHash : string -> string [] -> XmlRpcStruct
        
        [<XmlRpcMethod("TryUploadSubtitles")>]
        abstract TryUploadSubtitles : string -> XmlRpcStruct [] -> XmlRpcStruct
        
        [<XmlRpcMethod("UploadSubtitles")>]
        abstract UploadSubtitles : string -> XmlRpcStruct -> XmlRpcStruct

        [<XmlRpcMethod("SuggestMovie")>]
        abstract SuggestMovie : string -> string -> XmlRpcStruct
    
        [<XmlRpcMethod("ReportWrongMovieHash")>]
        abstract ReportWrongMovieHash : string -> string -> XmlRpcStruct

    let Proxy = XmlRpcProxyGen.Create<IStateName>()    
    Proxy.EnableCompression <- true

    let inline tryGetStringValue prop (source : XmlRpcStruct) = 
        if (source.ContainsKey(prop)) then string source.[prop]
        else "property not found"
    
    let inline tryGetArrayValue (prop:string) (source : XmlRpcStruct) = 
        if source.ContainsKey(prop) then 
            match source.[prop] with
            | :? (XmlRpcStruct[]) as data -> data
            | _ -> Array.empty<XmlRpcStruct>
        else Array.empty<XmlRpcStruct>

    let StartLogging() = Proxy.AttachLogger(RequestResponseLogger())
