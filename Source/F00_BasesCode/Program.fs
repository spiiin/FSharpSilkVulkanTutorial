open Silk.NET.Maths
open Silk.NET.Windowing
open System

type HelloTriangleApplication() =
    static let WIDTH = 800
    static let HEIGHT = 600

    let mutable window: IWindow option = None

    member this.Run() =
        this.InitWindow()
        this.InitVulkan()
        this.MainLoop()
        this.CleanUp()

    member private this.InitWindow() =
        let mutable options = WindowOptions.DefaultVulkan
        options.Size <- Vector2D<int>(WIDTH, HEIGHT)
        options.Title <- "Vulkan"

        window <- Some(Window.Create(options))
        window.Value.Initialize()

        match window.Value.VkSurface with
        | null -> raise (Exception("Windowing platform doesn't support Vulkan."))
        | _ -> ()

    member private this.InitVulkan() = ()

    member private this.MainLoop() = window.Value.Run()

    member private this.CleanUp() =
        match window with
        | Some w -> w.Dispose()
        | None -> ()

[<EntryPoint>]
let main _ =
    HelloTriangleApplication().Run()
    0