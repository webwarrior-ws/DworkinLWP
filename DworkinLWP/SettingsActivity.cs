using Android.App;
using Android.Content;
using Android.OS;
using Android.Runtime;
using Android.Views;
using Android.Widget;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace DworkinLWP
{
    [Activity(Name="dworkinLWP.SettingsActivity", Label = "Dworkin LWP")]
    class SettingsActivity : DworkinUI.SettingsActivity
    {
        public override void OnCreate(Bundle bundle)
        {
            DworkinWallpaperService.EnsureCurrentSettings(this);
            if (bundle == null) // TODO: see if this is no longer needed
            {
                Settings.XamarinCrutch.updateLibraryResourceIds(typeof(Resource), typeof(DworkinUI.Resources));
                Settings.XamarinCrutch.updateLibraryResourceIds(typeof(Resource), typeof(Settings.Resources));
            }
            base.OnCreate(bundle);
        }

        protected override void OnStop()
        {
            base.OnStop();
            LibDworkin.DworkinSettings.CurrentSettings.save(this);
        }
    }
}