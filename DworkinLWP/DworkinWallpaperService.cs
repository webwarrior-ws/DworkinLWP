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

using static LibDworkin.DworkinSettings;
using Microsoft.Maui.ApplicationModel;


namespace DworkinLWP
{
    [Service(Name = "dworkinLWP.DworkinWallpaperService", Enabled = true, Label = "@string/app_name", Permission = "android.permission.BIND_WALLPAPER", Exported = true)]
    [IntentFilter(new string[] { "android.service.wallpaper.WallpaperService" })]
    [MetaData("android.service.wallpaper", Resource = "@xml/wallpaper")]
    public class DworkinWallpaperService : WallpaperService
    {
        static private LibDworkin.RendererComponents.AndroidPlatform platformContainer;

        static public void EnsureCurrentSettings(Context context)
        {
            if (CurrentSettings.isSet()) return;

            DworkinUI.CurrentPresetSelection.installHooks();
            var _ = ConfigSettings.allEnvironmentSettingsTypes; // force registering environment settings types
            CurrentSettings.load(context);
            if (!CurrentSettings.isSet())
            {
                var defaultSettings = new SettingsHolder(
                        new ConfigSettings.FlowingLeaves(),
                        new Effects.Plain(),
                        new Settings.Randomizeable<Tuple<float, float, float>>.Defined(new Tuple<float, float, float>(0.1f, 0.1f, 0.1f)),
                        new Settings.Randomizeable<Tuple<float, float, float>>.Defined(new Tuple<float, float, float>(0.95f, 0.95f, 0.95f)),
                        new Materials.SolidColor(),
                        new Materials.SolidColor(),
                        1054841354);
                CurrentSettings.set(defaultSettings);
            }
        }

        internal LibDworkin.RendererComponents.AndroidPlatform Platform
        {
            get
            {
                if (platformContainer == null)
                {
                    platformContainer = new LibDworkin.RendererComponents.AndroidPlatform(this.ApplicationContext);
                }
                return platformContainer;
            }
        }

        public override void OnCreate()
        {
            base.OnCreate();
            DworkinWallpaperService.EnsureCurrentSettings(this.ApplicationContext);
        }

        public override void OnDestroy()
        {
            CurrentSettings.save(this.ApplicationContext);
            base.OnDestroy();
        }

        public override WallpaperService.Engine OnCreateEngine()
        {
            return new DworkinEngine(this);
        }

        [Register("dowrkinLWP/DworkinWallpaperService$DworkinEngine", DoNotGenerateAcw = true)]
        class DworkinEngine(DworkinWallpaperService __self) : Engine(__self)
        {
            class WallpaperGLSurfaceView : GLSurfaceView
            {
                private static DworkinEngine tempEngine;
                private static readonly Object syncObject = new Object();
                private readonly DworkinEngine engine;

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

            private WallpaperGLSurfaceView glSurfaceView;
            private bool rendererSet;
            private GestureDetector gestureListener;

            private class GestureListener : GestureDetector.SimpleOnGestureListener
            {
                public override bool OnDoubleTap(MotionEvent e)
                {
                    CurrentSettings.changeSeed(new System.Random().Next());
                    return base.OnDoubleTap(e);
                }
            }

            public override void OnCreate(ISurfaceHolder surfaceHolder)
            {
                base.OnCreate(surfaceHolder);
                
                glSurfaceView = WallpaperGLSurfaceView.Create(__self, this);

                var isPreview = Microsoft.FSharp.Core.FSharpOption<bool>.Some(this.IsPreview);
                var renderer = new LibDworkin.Renderer.DworkinRenderer(__self.Platform, null, isPreview);

                glSurfaceView.SetEGLContextClientVersion(2);
                glSurfaceView.SetEGLContextFactory(LibDworkin.RendererComponents.EGLContextFactory.Instance);
                glSurfaceView.PreserveEGLContextOnPause = true;
                glSurfaceView.SetRenderer(renderer);
                rendererSet = true;

                gestureListener = new GestureDetector(__self.ApplicationContext, new GestureListener());
                SetTouchEventsEnabled(true);
            }

            public override void OnTouchEvent(MotionEvent evt) 
            {
                gestureListener.OnTouchEvent(evt);
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