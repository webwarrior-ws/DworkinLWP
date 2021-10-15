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
        const int REQUEST_CODE_CHANGE_WALLPAPER = 0x1;

        public override void OnCreate(Bundle bundle)
        {
            DworkinWallpaperService.EnsureCurrentSettings(this);
            _ = DworkinUI.CustomEnumEditors.RegisterCustomEditors.Value;
            if (bundle == null) // TODO: see if this is no longer needed
            {
                Settings.XamarinCrutch.updateLibraryResourceIds(typeof(Resource), typeof(DworkinUI.Resources));
                Settings.XamarinCrutch.updateLibraryResourceIds(typeof(Resource), typeof(Settings.Resources));
            }
            Xamarin.Essentials.Platform.Init(this, bundle);
            base.OnCreate(bundle);
        }

        protected override void OnStop()
        {
            base.OnStop();
            LibDworkin.DworkinSettings.CurrentSettings.save(this);
        }

        public override bool OnCreateOptionsMenu(IMenu menu)
        {
            this.MenuInflater.Inflate(Resource.Menu.menu, menu);
            return true;
        }
        
        public override bool OnOptionsItemSelected(IMenuItem item)
        {
            if (item.ItemId == Resource.Id.action_set_wallpaper)
            {
                Intent intent = new Intent(WallpaperManager.ActionChangeLiveWallpaper);
                var lwpServiceJavaClass = Java.Lang.Class.FromType(typeof(DworkinWallpaperService));
                intent.PutExtra(WallpaperManager.ExtraLiveWallpaperComponent, new ComponentName(this, lwpServiceJavaClass));
                this.StartActivityForResult(intent, REQUEST_CODE_CHANGE_WALLPAPER);
                return true;
            }
            else
                return base.OnOptionsItemSelected(item);
        }

        protected override void OnActivityResult(int requestCode, [GeneratedEnum] Result resultCode, Intent data)
        {
            base.OnActivityResult(requestCode, resultCode, data);
            if (requestCode == REQUEST_CODE_CHANGE_WALLPAPER && resultCode == Result.Ok)
            {
                this.Finish();
            }
        }
    }
}