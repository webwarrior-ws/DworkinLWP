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


namespace DworkinLWP
{
    [Service(Name = "dworkinLWP.DworkinWallpaperService", Enabled = true, Label = "@string/app_name", Permission = "android.permission.BIND_WALLPAPER")]
    [IntentFilter(new string[] { "android.service.wallpaper.WallpaperService" })]
    [MetaData("android.service.wallpaper", Resource = "@xml/wallpaper")]
    class DworkinWallpaperService : WallpaperService
    {
        static private LibDworkin.RendererComponents.AndroidPlatform platformContainer;
        static private Microsoft.FSharp.Core.FSharpRef<SettingsHolder> settingsHolder;

        static public void EnsureCurrentSettings(Context context)
        {
            if (CurrentSettings.isSet()) return;

            DworkinUI.CurrentPresetSelection.installHooks();
            //CurrentSettings.load(context);
            if (!CurrentSettings.isSet())
            {
                var defaultSettings = new SettingsHolder(
                        new ConfigSettings.PCB_RandomPosChips(),
                        new Effects.Plain(),
                        new Settings.Randomizeable<Tuple<float, float, float>>.Defined(new Tuple<float, float, float>(0.0f, 0.0f, 0.0f)),
                        new Settings.Randomizeable<Tuple<float, float, float>>.Defined(new Tuple<float, float, float>(1.0f, 1.0f, 1.0f)),
                        new Materials.SolidColor(),
                        new Materials.SolidColor(),
                        42);
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

        class DworkinEngine : Engine
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

            private readonly DworkinWallpaperService enclosingInstance;
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

                settingsHolder = new Microsoft.FSharp.Core.FSharpRef<SettingsHolder>(CurrentSettings.get());
                var renderer = new LibDworkin.Renderer.DworkinRenderer( 
                    enclosingInstance.Platform,
                    settingsHolder,
                    null);

                glSurfaceView.SetEGLContextClientVersion(2);
                glSurfaceView.SetEGLContextFactory(LibDworkin.Renderer.EGLContextFactory.Instance);
                glSurfaceView.PreserveEGLContextOnPause = true;
                glSurfaceView.SetRenderer(renderer);
                rendererSet = true;
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
                    {
                        settingsHolder.Value = CurrentSettings.get();
                        glSurfaceView.OnResume();
                    }
                    else
                        glSurfaceView.OnPause();
                }

            }
        }
    }
}