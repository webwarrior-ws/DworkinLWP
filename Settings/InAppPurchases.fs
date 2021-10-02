module Settings.InAppPurchases

open Android.Content


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


let mutable private purchasedItems = Set.empty<string>
let mutable private purchasesInfo = Map.empty<string, IAPItemInfo>

// setPurchases?
let addPurchases(skus : string[]) : unit =
    for item in skus do 
        purchasedItems <- purchasedItems.Add item


let private sharedPrefsName = "iaps"
let private purchasedItemsPrefsKey = "purchased_items"

/// Write purchased items from memory to storage
let writeToStorage(context : Context) : unit =
    let prefs = context.GetSharedPreferences(sharedPrefsName, FileCreationMode.Private)
    let editor = prefs.Edit()
    editor.PutStringSet(purchasedItemsPrefsKey, purchasedItems) |> ignore

/// Read purchased items from storage to memory
let readFromStorage(context : Context) : unit =
    let prefs = context.GetSharedPreferences(sharedPrefsName, FileCreationMode.Private)
    if prefs.Contains(purchasedItemsPrefsKey) then
        purchasedItems <- Set.ofSeq(prefs.GetStringSet(purchasedItemsPrefsKey, [||]))


let isPurchased(sku : string) : bool =
    purchasedItems |> Set.contains sku

let getAvailablePurchases() : string[] =
    IAPItem.all 
    |> Seq.except purchasedItems
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
            

/// Access purchased in-app purchases using Google Play Billing API and store results in memeory and storage.
/// If Billing API returns error, use values stored in storage.
let refreshPurchasesInfo(context : Context) =
    // async?
    (*let billing = CrossInAppBilling.Current
    try
    { 
        var connected = await billing.ConnectAsync();

        if (!connected)
        {
            //Couldn't connect
            return false;
        }

        //check purchases
        var purchases = await billing.GetPurchasesAsync(ItemType.InAppPurchase);

        //check for null just incase
        if(purchases?.Any(p => p.ProductId == productId) ?? false)
        {
            //Purchase restored
            return true;
        }
        else
        {
            //no purchases found
            return false;
        }
    }    
    catch (InAppBillingPurchaseException purchaseEx)
    {
        //Billing Exception handle this based on the type
        Debug.WriteLine("Error: " + purchaseEx);
    }
    catch (Exception ex)
    {
        //Something has gone wrong
    }
    finally   
        await billing.DisconnectAsync();

    false *)
    ()
    //TODO: implement


let getPurchasesDialog(context: Context, skus: string[]) : Android.App.Dialog =
    failwith "not yet implemented"