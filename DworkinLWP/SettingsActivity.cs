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
        static SettingsActivity()
        {
            DworkinUI.ActivityWithBackButton.MenuComponent = new MenuComponent();
        }

        public override void OnCreate(Bundle bundle)
        {
            DworkinWallpaperService.EnsureCurrentSettings(this);
            _ = DworkinUI.CustomEnumEditors.RegisterCustomEditors.Value;
            if (bundle == null) // TODO: see if this is no longer needed
            {
                Settings.XamarinCrutch.updateLibraryResourceIds(typeof(Resource), typeof(DworkinUI.Resources));
                Settings.XamarinCrutch.updateLibraryResourceIds(typeof(Resource), typeof(Settings.Resources));
            }
            base.OnCreate(bundle);
            Xamarin.Essentials.Platform.Init(this, bundle);
        }

        protected override void OnStop()
        {
            base.OnStop();
            LibDworkin.DworkinSettings.CurrentSettings.save(this);
        }
    }

    class MenuComponent : DworkinUI.IMenuComponent
    {
        const int REQUEST_CODE_CHANGE_WALLPAPER = 0x1;

        public bool OnCreateOptionsMenu(AndroidX.AppCompat.App.AppCompatActivity activity, IMenu menu)
        {
            activity.MenuInflater.Inflate(Resource.Menu.menu, menu);
            return true;
        }

        public bool OnOptionsItemSelected(AndroidX.AppCompat.App.AppCompatActivity activity, IMenuItem item)
        {
            if (item.ItemId == Resource.Id.action_set_wallpaper)
            {
                Intent intent = new Intent(WallpaperManager.ActionChangeLiveWallpaper);
                var lwpServiceJavaClass = Java.Lang.Class.FromType(typeof(DworkinWallpaperService));
                intent.PutExtra(WallpaperManager.ExtraLiveWallpaperComponent, new ComponentName(activity, lwpServiceJavaClass));
                activity.StartActivityForResult(intent, REQUEST_CODE_CHANGE_WALLPAPER);
                return true;
            }
            else
                return false;
        }

        public void OnActivityResult(AndroidX.AppCompat.App.AppCompatActivity activity, int requestCode, [GeneratedEnum] Result resultCode)
        {
            if (requestCode == REQUEST_CODE_CHANGE_WALLPAPER && resultCode == Result.Ok)
            {
                activity.Finish();
            }
        }
    }

    // trick the linker
    class LinkerPleaseInclude
    {
        static bool falseflag = false;
        static LinkerPleaseInclude()
        {
            if (falseflag)
            {
                var ignore = new AndroidX.AppCompat.Widget.FitWindowsFrameLayout(Application.Context);
            }
        }
    }
}

