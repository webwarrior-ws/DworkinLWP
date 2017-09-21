using System;
using System.Collections.Generic;

using Android.App;
using Android.Content;
using Android.OS;
using Android.Runtime;
using Android.Views;
using Android.Widget;
using Android.Service.Wallpaper;
using Android.Opengl;
using Javax.Microedition.Khronos.Egl;
using Javax.Microedition.Khronos.Opengles;

namespace DworkinLWP
{
    [Service(Label = "@string/app_name", Permission = "android.permission.BIND_WALLPAPER")]
    [IntentFilter(new string[] { "android.service.wallpaper.WallpaperService" })]
    [MetaData("android.service.wallpaper", Resource = "@xml/wallpaper")]
    class DworkinWallpaperService : WallpaperService
    {
        public override WallpaperService.Engine OnCreateEngine()
        {
            return new DworkinEngine(this);
        }

        class DworkinEngine : Engine
        {
            class WallpaperGLSurfaceView : GLSurfaceView
            {
                private static DworkinEngine tempEngine;
                private static Object syncObject = new Object();
                private DworkinEngine engine;

                public static WallpaperGLSurfaceView Create(Context context, DworkinEngine engine)
                {
                    // ugly hack for engine to be accessible before base constructor is called
                    lock (syncObject) 
                    {
                        tempEngine = engine;
                        return new WallpaperGLSurfaceView(context, engine);
                    }
                }

                private WallpaperGLSurfaceView(Context context, DworkinEngine engine) : base(context)
                {
                    this.engine = engine;
                    tempEngine = null;
                }

                public override ISurfaceHolder Holder { get => (engine ?? tempEngine).SurfaceHolder; }

                public void OnWallpaperDestroy()
                {
                    base.OnDetachedFromWindow();
                }
            }

            private DworkinWallpaperService enclosingInstance;
            private WallpaperGLSurfaceView glSurfaceView;
            private bool rendererSet;

            public DworkinEngine(WallpaperService __self) : base(__self)
            {
                enclosingInstance = __self as DworkinWallpaperService;
            }

            public override void OnCreate(ISurfaceHolder surfaceHolder)
            {
                base.OnCreate(surfaceHolder);
                
                glSurfaceView = WallpaperGLSurfaceView.Create(enclosingInstance, this);

                // Check if the system supports OpenGL ES 2.0. 
                var activityManager = enclosingInstance.GetSystemService(Context.ActivityService) as ActivityManager;
                var configurationInfo = activityManager.DeviceConfigurationInfo;
                // Check for emulator. 
                var supportsEs2 = configurationInfo.ReqGlEsVersion >= 0x20000 
                    || (Build.VERSION.SdkInt >= BuildVersionCodes.IceCreamSandwichMr1 && (Build.Fingerprint.StartsWith("generic") 
                    || Build.Fingerprint.StartsWith("unknown") 
                    || Build.Model.Contains("google_sdk") 
                    || Build.Model.Contains("Emulator") 
                    || Build.Model.Contains("Android SDK built for x86")));
                var renderer = new LibDworkin.ForTesting.ShapesTestRenderer(enclosingInstance);
                if (supportsEs2)
                {
                    glSurfaceView.SetEGLContextClientVersion(2);
                    glSurfaceView.PreserveEGLContextOnPause = true;
                    glSurfaceView.SetRenderer(renderer);
                    rendererSet = true;
                }
                else {
                    Toast.MakeText(enclosingInstance, "This device does not support OpenGL ES 2.0.", ToastLength.Long).Show();
                    return;
                }
            }

            public override void OnDestroy()
            {
                base.OnDestroy();
                glSurfaceView.OnWallpaperDestroy();
            }

            public override void OnVisibilityChanged(bool visible)
            {
                base.OnVisibilityChanged(visible);
                if (rendererSet)
                {
                    if (visible)
                        glSurfaceView.OnResume();
                    else
                        glSurfaceView.OnPause();
                }

            }
        }
    }
}