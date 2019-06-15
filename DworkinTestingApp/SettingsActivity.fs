namespace DworkinTestingApp

open Android.App

open Settings


type Resources = DworkinTestingApp.Resource


[<Activity (Label = "SettingsActivity")>]
type SettingsActivity() =
    inherit Activity()

    override this.OnCreate(bundle) =
        base.OnCreate (bundle)
        // Create your application here



