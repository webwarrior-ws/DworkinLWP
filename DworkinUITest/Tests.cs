using System;
using System.IO;
using System.Linq;
using NUnit.Framework;
using Xamarin.UITest;
using Xamarin.UITest.Queries;

namespace DworkinUITest
{
    [TestFixture(Platform.Android)]
    public class Tests
    {
        IApp app;
        Platform platform;

        public Tests(Platform platform)
        {
            this.platform = platform;
        }

        public void WaitForTimeSpan(TimeSpan timeSpan)
        {
            try
            {
                app.WaitForElement(c => c.Text("BLAH BLAH BLAH"), "Waiting for something, killing time", timeSpan);
            }
            catch (Exception)
            {
                //do nothing we just wanted to kill some time
            }
        }

        [SetUp]
        public void BeforeEachTest()
        {
            app = AppInitializer.StartApp(platform);
        }

        [Test]
        public void WallpaperPreviewWithCustomSettings()
        {
            app.WaitForElement(c => c.Marked("action_set_wallpaper"));
            app.Invoke("ChangeAppearanceSettings");

            app.Screenshot("Settings screen");

            app.Tap(c => c.Marked("action_set_wallpaper"));

            this.WaitForTimeSpan(TimeSpan.FromSeconds(3.0));

            app.Screenshot("Wallpaper preview");
        }

        [Test]
        public void PresetSelectionScreens()
        {
            app.Invoke("ChangeAppearanceSettings");
            app.Tap(c => c.Marked("Appearance"));

            app.Tap(c => c.Marked("preset_control"));
            this.WaitForTimeSpan(TimeSpan.FromSeconds(1.0));
            app.Screenshot("Appearance presets");

            app.Tap(c => c.Marked("save_preset_button"));
            this.WaitForTimeSpan(TimeSpan.FromSeconds(0.2));
            app.Screenshot("Appearance presets save");

            app.Back();
            app.WaitForElement(c => c.Marked("save_preset_button"));
            app.Back();
            app.WaitForElement(c => c.Marked("preset_control"));
            app.Back();

            app.Tap(c => c.Marked("preset_control"));
            this.WaitForTimeSpan(TimeSpan.FromSeconds(1.0));
            app.Screenshot("Overall presets");

            app.Tap(c => c.Marked("save_preset_button"));
            this.WaitForTimeSpan(TimeSpan.FromSeconds(0.5));
            app.Screenshot("Overall presets save");
        }
    
    }
}
