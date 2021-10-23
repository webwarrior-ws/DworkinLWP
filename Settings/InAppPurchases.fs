module Settings.InAppPurchases

open Android.Content
open Android.App
open Android.Widget

open Plugin.InAppBilling

open Atom


type InAppPurchaseAttribute(sku : string) =
    inherit System.Attribute()
    member this.SKU = sku


module IAPItem =
    [<Literal>] 
    let EffectsAndMaterials = "effects_and_materials"
    [<Literal>] 
    let PatternPCB = "pattern_pcb"
    [<Literal>] 
    let PatternVine = "pattern_vine"
    [<Literal>] 
    let PatternFloralSwirls = "pattern_floral_swirls"

    let all = [| EffectsAndMaterials; PatternPCB; PatternVine; PatternFloralSwirls; |]


type IAPItemInfo = class end //TODO: replace by InAppBillingProduct


let private purchasedItems = Atom(Set.empty<string>)

// setPurchases?
let addPurchases(skus : string[]) : unit =
    for item in skus do 
        swap purchasedItems (Set.add item)


let private sharedPrefsName = "iaps"
let private purchasedItemsPrefsKey = "purchased_items"

/// Write purchased items from memory to storage
let writeToStorage(context : Context) : unit =
    let prefs = context.GetSharedPreferences(sharedPrefsName, FileCreationMode.Private)
    let editor = prefs.Edit()
    editor.PutStringSet(purchasedItemsPrefsKey, !purchasedItems) |> ignore
    editor.Commit() |> ignore

/// Read purchased items from storage to memory
let readFromStorage(context : Context) : unit =
    let prefs = context.GetSharedPreferences(sharedPrefsName, FileCreationMode.Private)
    if prefs.Contains(purchasedItemsPrefsKey) then
        let storedValue = prefs.GetStringSet(purchasedItemsPrefsKey, [||]) |> Set.ofSeq
        swap purchasedItems (fun _ -> storedValue)


let isPurchased(sku : string) : bool =
    !purchasedItems |> Set.contains sku

let getAvailablePurchases() : string[] =
    IAPItem.all 
    |> Seq.except !purchasedItems
    |> Seq.toArray


let private classSKUMapping = System.Collections.Generic.Dictionary<System.Type, string option>()

let getSKUForClass(cls : System.Type) : string option =
    match classSKUMapping.TryGetValue(cls) with
    | true, value -> value
    | false, _ ->
        let attribs = cls.GetCustomAttributes(typeof<InAppPurchaseAttribute>, true)
        let result =
            if Array.isEmpty attribs then
                None
            else
                Some((attribs.[0] :?> InAppPurchaseAttribute).SKU)
        classSKUMapping.[cls] <- result
        result
         
         
let private writeLog (msg : string) =
    Android.Util.Log.Debug("iaps", msg) |> ignore
    

let private performBillingAction<'T>(action: IInAppBilling -> Async<'T>) (fallback: Async<'T>) (timeout: int) : Async<'T> =
    async {
        let billing = CrossInAppBilling.Current
        try
            let connected = billing.ConnectAsync().Wait(timeout)
            if connected then
                writeLog("Connected to Billing API")
                return! action billing
            else 
                writeLog("Failed to connect to Billing API")
                return! fallback
        finally
            Async.RunSynchronously (billing.DisconnectAsync() |> Async.AwaitTask)
            writeLog("Disconnected from Billing API")
    }

let private getPurchaseItemsAsync() : Async<seq<InAppBillingPurchase>> =
    performBillingAction 
        (fun billing -> 
            async {
                let! purchases = billing.GetPurchasesAsync(ItemType.InAppPurchase) |> Async.AwaitTask
                //check for null just incase
                return (if not (isNull purchases) then purchases else Seq.empty)
            })
        (async { return Seq.empty })
        2000

/// Access purchased in-app purchases using Google Play Billing API and store results in memeory and storage.
/// If Billing API returns error, use values stored in storage.
let refreshPurchasesInfo(context: Context) =
    async {
        try
            let! purchases = getPurchaseItemsAsync()
            swap 
                purchasedItems
                (fun _ -> 
                 purchases 
                 |> Seq.filter (fun p -> p.State = PurchaseState.Purchased) // ?
                 |> Seq.map (fun p -> p.ProductId)
                 |> Set.ofSeq)
            writeToStorage(context)
        with
        | ex ->
            writeLog(sprintf "Error in refreshPurchasesInfo: %A" ex)
            readFromStorage(context)
    }
    |> Async.Start


let getPurchasesDataAsync (skus : string[]) =
    performBillingAction
        (fun billing -> billing.GetProductInfoAsync(ItemType.InAppPurchase, skus) |> Async.AwaitTask)
        (async { return raise (System.Exception("Could not connect to Billing API")) })
        1000


/// Get information about available purchases among given skus 
/// and answer dialog containing purchase actions
let getPurchasesDialog(context: Context, purchasesData : seq<InAppBillingProduct>) : Dialog =
    let errorDialog(message: string) =
        use builder = new AlertDialog.Builder(context)
        builder.SetNeutralButton("OK", (fun _ _ -> ())) |> ignore
        builder.SetTitle("Error").SetMessage(message).Create()

    try
        use builder = new AlertDialog.Builder(context)
        let dialogView = new LinearLayout(context, Orientation=Orientation.Vertical)
        builder
            .SetView(dialogView)
            .SetTitle("Buy premium content")
            .SetNegativeButton("Cancel", (fun _ _ -> ())) |> ignore
        let dialog = builder.Create()

        for each in purchasesData do
            let button = new Button(context)
            button.Text <- sprintf "Buy %s (%s)" each.Description each.LocalizedPrice
            let buyItemAsync = 
                performBillingAction
                    (fun billing -> async {
                        let! purchase = billing.PurchaseAsync(each.ProductId, ItemType.InAppPurchase) |> Async.AwaitTask // TODO: verify
                        dialog.Cancel()
                        if not (isNull purchase) && purchase.State = PurchaseState.Purchased then
                            swap purchasedItems (Set.add purchase.ProductId)
                            writeToStorage(context)
                            if not purchase.IsAcknowledged then
                                let! purchaseAcknowledged = billing.AcknowledgePurchaseAsync(purchase.PurchaseToken) |> Async.AwaitTask
                                if not purchaseAcknowledged then
                                    swap purchasedItems (Set.remove purchase.ProductId)
                                    writeToStorage(context)
                        })
                    (async { (errorDialog "Could not connect to Billing API").Show() |> ignore })
                    1000
            button.Click.Add(fun args -> Async.Start buyItemAsync)
            dialogView.AddView(button)

        dialog :> Dialog 
    with
    | ex ->
        writeLog(sprintf "Error in getPurchasesDialog: %A" ex)
        errorDialog ex.Message :> Dialog