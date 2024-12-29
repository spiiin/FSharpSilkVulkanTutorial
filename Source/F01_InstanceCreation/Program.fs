open Silk.NET.Maths
open Silk.NET.Vulkan
open Silk.NET.Windowing
open Silk.NET.Core.Native
open System
open System.Runtime.InteropServices;
open FSharp.NativeInterop

#nowarn "9"
#nowarn "0051"

type HelloTriangleApplication() =
    static let WIDTH = 800
    static let HEIGHT = 600

    let mutable window: IWindow option = None
    let mutable vk: Vk option = None
    let mutable instance: Instance = Unchecked.defaultof<Instance>
    let pNullAllocator: nativeptr<AllocationCallbacks> = NativePtr.nullPtr

    member this.Run() =
        this.InitWindow()
        this.InitVulkan()
        this.MainLoop()
        this.CleanUp()

    member private this.InitWindow() =
        let mutable options = WindowOptions.DefaultVulkan
        options.Size <- Vector2D<int>(WIDTH, HEIGHT)
        options.Title <- "Vulkan"

        window <- Some <| Window.Create(options)
        window.Value.Initialize()

        match window.Value.VkSurface with
        | null -> raise <| Exception("Windowing platform doesn't support Vulkan.")
        | _ -> ()

    member private this.InitVulkan() = this.CreateInstance()

    member private this.MainLoop() = 
        match window with
        | Some w -> w.Run()
        | None -> raise <| InvalidOperationException("Window is not initialized.")

    member private this.CleanUp() =
        match vk with
        | Some vkApi ->
            vkApi.DestroyInstance(instance, pNullAllocator)
            vkApi.Dispose()
        | None -> ()

        match window with
        | Some w -> w.Dispose()
        | None -> ()

    member private this.CreateInstance() =
        vk <- Some <| Vk.GetApi()

        let mutable appInfo = ApplicationInfo()
        appInfo.SType <- StructureType.ApplicationInfo
        appInfo.PApplicationName <- NativePtr.ofNativeInt <| Marshal.StringToHGlobalAnsi("Hello Triangle")
        appInfo.ApplicationVersion <- uint32 <| Silk.NET.Core.Version32(1u,0u,0u)
        appInfo.PEngineName <- NativePtr.ofNativeInt <| Marshal.StringToHGlobalAnsi("No Engine")
        appInfo.EngineVersion <- uint32 <| Silk.NET.Core.Version32(1u,0u,0u)
        appInfo.ApiVersion <- uint32 Vk.Version12 

        let mutable glfwExtensionCount = 0u
        let glfwExtensions = window.Value.VkSurface.GetRequiredExtensions(&glfwExtensionCount)

        let mutable createInfo = InstanceCreateInfo()
        createInfo.SType <- StructureType.InstanceCreateInfo
        createInfo.PApplicationInfo <- &&appInfo
        createInfo.EnabledExtensionCount <- glfwExtensionCount
        createInfo.PpEnabledExtensionNames <- glfwExtensions
        createInfo.EnabledLayerCount <- 0u

        match vk.Value.CreateInstance(&&createInfo, pNullAllocator, &instance) with
        | Result.Success -> ()
        | _ -> raise <| Exception("Failed to create instance!")

        Marshal.FreeHGlobal <| NativePtr.toNativeInt appInfo.PApplicationName
        Marshal.FreeHGlobal <| NativePtr.toNativeInt appInfo.PEngineName

[<EntryPoint>]
let main _ =
    HelloTriangleApplication().Run()
    0