open Silk.NET.Maths
open Silk.NET.Vulkan
open Silk.NET.Windowing
open Silk.NET.Vulkan.Extensions.EXT
open Silk.NET.Core.Native
open System
open System.Runtime.InteropServices
open FSharp.NativeInterop

#nowarn "9"
#nowarn "0051"

type QueueFamilyIndices() =
    member val GraphicsFamily: uint32 option = None with get, set
    member this.IsComplete() = this.GraphicsFamily.IsSome

let inline vkQueryAndFillArray<^T when ^T : unmanaged> (fillArray: ref<uint32> -> nativeptr<^T> -> unit) =
    let count = ref 0u
    fillArray count NativePtr.nullPtr
    let array = Array.zeroCreate<^T> (int !count)
    use ptr = fixed array
    fillArray count ptr
    array

type HelloTriangleApplication() =
    static let WIDTH = 800
    static let HEIGHT = 600

    let mutable window: IWindow option = None
    let mutable vk: Vk option = None
    let mutable instance: Instance = Unchecked.defaultof<Instance>

    let mutable debugUtils: ExtDebugUtils option = None
    let mutable debugMessenger: DebugUtilsMessengerEXT = Unchecked.defaultof<DebugUtilsMessengerEXT>

    let mutable physicalDevice: PhysicalDevice = Unchecked.defaultof<PhysicalDevice>

    let mutable device: Device = Unchecked.defaultof<Device>
    let mutable graphicsQueue: Queue = Unchecked.defaultof<Queue>

    let pNullAllocator: nativeptr<AllocationCallbacks> = NativePtr.nullPtr
    let validationLayers = [| "VK_LAYER_KHRONOS_validation" |]

    let mutable enableValidationLayers = true

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

    member private this.InitVulkan() =
        this.CreateInstance()
        this.SetupDebugMessenger()
        this.PickPhysicalDevice()
        this.CreateLogicalDevice()

    member private this.MainLoop() = 
        match window with
        | Some w -> w.Run()
        | None -> raise <| InvalidOperationException("Window is not initialized.")

    member private this.CleanUp() =
        match vk with
        | Some vkApi ->
            vkApi.DestroyDevice(device, pNullAllocator)
            if enableValidationLayers then
                debugUtils.Value.DestroyDebugUtilsMessenger(instance, debugMessenger, pNullAllocator)
            vkApi.DestroyInstance(instance, pNullAllocator)
            vkApi.Dispose()
        | None -> ()
        
        match window with
        | Some w -> w.Dispose()
        | None -> ()

    member private this.CreateInstance() =
        vk <- Some <| Vk.GetApi()

        if enableValidationLayers && not (this.CheckValidationLayerSupport()) then
            raise <| Exception("Validation layers requested, but not available!")

        let mutable appInfo = ApplicationInfo()
        appInfo.SType <- StructureType.ApplicationInfo
        appInfo.PApplicationName <- NativePtr.ofNativeInt <| Marshal.StringToHGlobalAnsi("Hello Triangle")
        appInfo.ApplicationVersion <- uint32 <| Silk.NET.Core.Version32(1u, 0u, 0u)
        appInfo.PEngineName <- NativePtr.ofNativeInt <| Marshal.StringToHGlobalAnsi("No Engine")
        appInfo.EngineVersion <- uint32 <| Silk.NET.Core.Version32(1u, 0u, 0u)
        appInfo.ApiVersion <- uint32 Vk.Version12

        let mutable createInfo = InstanceCreateInfo()
        createInfo.SType <- StructureType.InstanceCreateInfo
        createInfo.PApplicationInfo <- &&appInfo

        let extensions : string array = this.GetRequiredExtensions()
        createInfo.EnabledExtensionCount <- uint32 (extensions.Length)
        createInfo.PpEnabledExtensionNames <- NativePtr.ofNativeInt <| SilkMarshal.StringArrayToPtr(extensions)

        if enableValidationLayers then
            createInfo.EnabledLayerCount <- uint32 validationLayers.Length
            createInfo.PpEnabledLayerNames <- NativePtr.ofNativeInt <| SilkMarshal.StringArrayToPtr(validationLayers)
            let mutable debugCreateInfo = this.PopulateDebugMessengerCreateInfo()
            createInfo.PNext <- NativePtr.toVoidPtr &&debugCreateInfo
        else
            createInfo.EnabledLayerCount <- 0u

        match vk.Value.CreateInstance(&createInfo, pNullAllocator, &instance) with
        | Result.Success -> ()
        | _ -> raise <| Exception("Failed to create instance!")

        Marshal.FreeHGlobal <| NativePtr.toNativeInt appInfo.PApplicationName
        Marshal.FreeHGlobal <| NativePtr.toNativeInt appInfo.PEngineName

    member private this.SetupDebugMessenger() =
        if enableValidationLayers then
            match vk.Value.TryGetInstanceExtension<ExtDebugUtils>(instance) with
            | true, extDebugUtils -> debugUtils <- Some extDebugUtils
            | false, _ -> raise <| Exception("DebugUtils extension is not available!")
        let mutable createInfo = this.PopulateDebugMessengerCreateInfo()
        match debugUtils.Value.CreateDebugUtilsMessenger(instance, &createInfo, pNullAllocator, &debugMessenger) with
        | Result.Success -> ()
        | _ -> raise <| Exception("Failed to set up debug messenger!")

    member private this.GetRequiredExtensions() =
        let glfwExtensions = window.Value.VkSurface.GetRequiredExtensions(ref 0u)
        let extensions = SilkMarshal.PtrToStringArray(NativePtr.toNativeInt glfwExtensions, 0)

        if enableValidationLayers then
            extensions |> Array.append [| ExtDebugUtils.ExtensionName |]
        else
            extensions

    member private this.PopulateDebugMessengerCreateInfo() =
        let mutable createInfo = DebugUtilsMessengerCreateInfoEXT()
        createInfo.SType <- StructureType.DebugUtilsMessengerCreateInfoExt
        createInfo.MessageSeverity <- DebugUtilsMessageSeverityFlagsEXT.VerboseBitExt ||| DebugUtilsMessageSeverityFlagsEXT.WarningBitExt ||| DebugUtilsMessageSeverityFlagsEXT.ErrorBitExt
        createInfo.MessageType <- DebugUtilsMessageTypeFlagsEXT.GeneralBitExt ||| DebugUtilsMessageTypeFlagsEXT.PerformanceBitExt ||| DebugUtilsMessageTypeFlagsEXT.ValidationBitExt
        createInfo.PfnUserCallback <- new PfnDebugUtilsMessengerCallbackEXT(this.DebugCallback)
        createInfo

    member private this.DebugCallback messageSeverity messageTypes pCallbackData pUserData =
        let message = Marshal.PtrToStringAnsi <| NativePtr.toNativeInt (NativePtr.read pCallbackData).PMessage
        printfn $"Validation Layer: {message}" 
        Vk.False

    member private this.CheckValidationLayerSupport() =
        let availableLayers = vkQueryAndFillArray(fun count arr -> vk.Value.EnumerateInstanceLayerProperties(count, arr) |> ignore)
        let availableLayerNames =
            availableLayers
            |> Array.map (fun layer -> 
                use pt = fixed &layer.LayerName
                Marshal.PtrToStringAnsi(NativePtr.toNativeInt(pt))
            )
            |> Set.ofArray
        validationLayers |> Array.forall (fun layer -> availableLayerNames.Contains(layer))

    member private this.PickPhysicalDevice() =
        let devices = vk.Value.GetPhysicalDevices(instance)
        match Seq.toList devices |> List.tryFind this.IsDeviceSuitable with
            | Some(device) -> physicalDevice <- device
            | None -> raise <| Exception("failed to find a suitable GPU!")

    member private this.IsDeviceSuitable(device: PhysicalDevice) =
        let indices = this.FindQueueFamilies(device)
        indices.IsComplete()

    member private this.FindQueueFamilies(device: PhysicalDevice) : QueueFamilyIndices =
        let mutable indices = QueueFamilyIndices()
        let queueFamilies = vkQueryAndFillArray(fun count arr -> vk.Value.GetPhysicalDeviceQueueFamilyProperties(device, count, arr)) |> Array.toList

        queueFamilies
        |> List.fold (fun (indices: QueueFamilyIndices, i) queueFamily ->
            if queueFamily.QueueFlags.HasFlag(QueueFlags.GraphicsBit) && not (indices.IsComplete()) then
                indices.GraphicsFamily <- Some i
            if indices.IsComplete() then indices, i else indices, i + 1u
        ) (indices, 0u)
        |> fun (finalIndices, _) -> finalIndices

    member private this.CreateLogicalDevice() =
        let indices = this.FindQueueFamilies(physicalDevice)

        let mutable queueCreateInfo = DeviceQueueCreateInfo()
        queueCreateInfo.SType <- StructureType.DeviceQueueCreateInfo
        queueCreateInfo.QueueFamilyIndex <- indices.GraphicsFamily.Value
        queueCreateInfo.QueueCount <- 1u

        let mutable queuePriority = 1.0f
        queueCreateInfo.PQueuePriorities <- &&queuePriority

        let mutable deviceFeatures = PhysicalDeviceFeatures()
        let mutable createInfo = DeviceCreateInfo()
        createInfo.SType <- StructureType.DeviceCreateInfo
        createInfo.QueueCreateInfoCount <- 1u
        createInfo.PQueueCreateInfos <- &&queueCreateInfo
        createInfo.PEnabledFeatures <- &&deviceFeatures
        createInfo.EnabledExtensionCount <- 0u

        if enableValidationLayers then
            createInfo.EnabledLayerCount <- uint32(validationLayers.Length)
            createInfo.PpEnabledLayerNames <- NativePtr.ofNativeInt <| SilkMarshal.StringArrayToPtr(validationLayers)
        else
            createInfo.EnabledLayerCount <- 0u

        match vk.Value.CreateDevice(physicalDevice, &createInfo, pNullAllocator, &device) with
        | Result.Success -> 
            vk.Value.GetDeviceQueue(device, indices.GraphicsFamily.Value, 0u, &graphicsQueue)
        | _ -> raise <| Exception("failed to create logical device!")

        if enableValidationLayers then
            SilkMarshal.Free <| NativePtr.toNativeInt createInfo.PpEnabledLayerNames |> ignore

[<EntryPoint>]
let main _ =
    HelloTriangleApplication().Run()
    0