using System;
using Xamarin.UITest;
using Xamarin.UITest.Queries;

namespace DworkinUITest
{
    public class AppInitializer
    {
        public static IApp StartApp(Platform platform)
        {
            return ConfigureApp.Android.ApkFile("D:/Projects/DworkinLWP/DworkinLWP/bin/Debug/dworkinLWP.DworkinLWP.apk")
                    .EnableLocalScreenshots()
                    .StartApp();
        }
    }
}