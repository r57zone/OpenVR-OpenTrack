[![EN](https://user-images.githubusercontent.com/9499881/33184537-7be87e86-d096-11e7-89bb-f3286f752bc6.png)](https://github.com/r57zone/OpenVR-OpenTrack/blob/master/README.md) 
[![RU](https://user-images.githubusercontent.com/9499881/27683795-5b0fbac6-5cd8-11e7-929c-057833e01fb1.png)](https://github.com/r57zone/OpenVR-OpenTrack/blob/master/README.RU.md) 
# OpenVR OpenTrack
OpenVR / SteamVR driver allowing head tracking with any [OpenTrack](https://github.com/opentrack/opentrack) trackers, for DIY VR headset made of Android smartphone or [HDMI display](http://ali.pub/1llt51) and tracker.<br>
<br>OpenTrack supports the following trackers: FreePie UDP receiver (FreePie IMU for Android), Hatire Arduino or [Razor IMU](https://github.com/Razor-AHRS/razor-9dof-ahrs) ([Arduino](http://ali.pub/1lltzk) + [GY-85](http://ali.pub/1lltk0)), [Oculus DK1](http://ali.pub/1llqtf), Aruco (Paper + [WebCam](http://ali.pub/2k9jf6)) and etc.<br>
<br>[![youtube-freetrack](https://user-images.githubusercontent.com/9499881/32277549-411d313c-bf2c-11e7-9b07-77a903783cf5.gif)](https://youtu.be/mDkdj_vn5Lk)

## Setup
1. Unpack SteamVR FreeTrack or UDP driver to folder "...\Steam\steamapps\common\SteamVR\drivers" (both are supported in OpenTrack, I recommend using FreeTrack). If necessary, configure the settings in the configuration file "...\Steam\steamapps\common\SteamVR\drivers\opentrack\resources\settings\default.vrsettings".
2. Download, install and configure [OpenTrack](https://github.com/opentrack/opentrack) (add hot centering key, disable filter, change output interface "freetrack 2.0 Enhanced" or "UDP over network", depending on the selected driver). If you selected UDP, then in the output interface settings you need to set IP "127.0.0.1".<br><br>

If you are using an Android smartphone, you need to use FreePie IMU from the OpenTrack archive to tracking and you can use the "Moonlight" application (for Nvidia 600 series only and above) or for any other application to stream pictures from the monitor screen. You can use this [guide](https://stackoverflow.com/a/46433454).<br><br>
If you use the [Arduino Razor IMU tracker](https://github.com/Razor-AHRS/razor-9dof-ahrs), you can use the standard [TrueOpenVR](https://github.com/TrueOpenVR), with its SteamVR driver or the [Razor IMU SteamVR](https://github.com/r57zone/VR-tracking-apps/releases) application.

## Configuration file options
Name | Description
------------ | -------------
DebugMode | debug mode, locked at 30 FPS. After checking, it is recommended to set it to false (disable).
DistanceBetweenEyes | the distance between stereo images, the larger the closer.
DistortionK1, DistortionK2 | lens distortion factors.
ScreenOffsetX | horizontal image shift.
ZoomHeight, ZoomWidth | scaling factors of stereo images.
displayFrequency | screen refresh rate.
renderWidth, renderHeight | image rendering resolution.
windowWidth, windowHeight | height and width of the displayed window.
windowX, windowY | window offset is required for display on other monitors. For example, to display on the second display, which is displayed on the left, you need to specify the value 1920 (provided that the first display we have is 1920 by 1080).

## Known Issues
1. Red screen. You can fix this by selecting the "Headset Window" window.
2. The keyboard, mouse and gamepad do not work when using the driver on one monitor (The game window should be in focus). On systems with multiple monitors, you can move the application to second monitor and select it, then everything will work.

## Download
>Version for x86 и x64.<br>
**[Download](https://github.com/r57zone/OpenVR-OpenTrack/releases)**

## Feedback
`r57zone[at]gmail.com`