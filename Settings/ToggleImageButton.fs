namespace Settings

// based on https://gist.github.com/akshaydashrath/9662072

open Android.App
open Android.Content
open Android.Util
open Android.Widget
open Android.OS
open Java.Interop

type private Resources = Settings.Resource


type ToggleImageButton(context : Context, attrs : IAttributeSet, defStyle : int) =
    inherit ImageButton(context, attrs, defStyle)
    
    static let CHECKED_STATE_SET = [| Android.Resource.Attribute.StateChecked |]

    let mutable isChecked = false
    let mutable isBroadCasting = false
    let mutable onCheckedChangeListener : ToggleImageButton * bool -> unit = ignore

    new(context, attrs) = new ToggleImageButton(context, attrs, 0)
    new(context) = new ToggleImageButton(context, null)

    member this.Initialize() =
        let a = context.ObtainStyledAttributes(attrs, Resources.Styleable.ToogleImageButton, defStyle, 0)
        (this :> ICheckable).Checked <- a.GetBoolean(Resources.Styleable.ToogleImageButton_checked, false)
        a.Recycle()

    member this.Checked
        with get() : bool = isChecked
        and set(value : bool) = 
            if value <> isChecked then
                isChecked <- value
                if isChecked then
                    this.SetColorFilter(Android.Graphics.Color.Navy)
                else
                    this.ClearColorFilter()
                this.RefreshDrawableState()
                if not isBroadCasting then
                    isBroadCasting <- true
                    onCheckedChangeListener(this, isChecked)
                    isBroadCasting <- false

    member this.Toggle() = this.Checked <- not this.Checked

    interface ICheckable with
        member this.Checked 
            with get() = this.Checked
            and set(value) = this.Checked <- value
        member this.Toggle() = this.Toggle()
    
    override this.OnFinishInflate() =
        this.Initialize()
        base.OnFinishInflate()

    override this.PerformClick() =
        this.Toggle()
        base.PerformClick()
    
    override this.OnCreateDrawableState(extraSpace : int) =
        let drawableState = base.OnCreateDrawableState(extraSpace + 1)
        if this.Checked then
            ToggleImageButton.MergeDrawableStates(drawableState, CHECKED_STATE_SET) |> ignore
        drawableState

    member this.OnCheckedChangeListener
        with set(listener) = onCheckedChangeListener <- listener

    override this.OnSaveInstanceState() =
        let bundle = new Bundle()
        bundle.PutParcelable("instanceState", base.OnSaveInstanceState())
        bundle.PutBoolean("state", isChecked)
        bundle :> IParcelable

    override this.OnRestoreInstanceState(state) =
        let newState : IParcelable =
            match state with
            | :? Bundle as outState ->
                this.Checked <- outState.GetBoolean("state")
                outState.GetParcelable("instanceState").JavaCast<IParcelable>()
            | _ -> state
        base.OnRestoreInstanceState(newState)
  