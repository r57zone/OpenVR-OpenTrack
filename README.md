[![EN](https://user-images.githubusercontent.com/9499881/33184537-7be87e86-d096-11e7-89bb-f3286f752bc6.png)](https://github.com/r57zone/OpenVR-OpenTrack/blob/master/README.md) 
[![RU](https://user-images.githubusercontent.com/9499881/27683795-5b0fbac6-5cd8-11e7-929c-057833e01fb1.png)](https://github.com/r57zone/OpenVR-OpenTrack/blob/master/README.RU.md) 
# OpenVR OpenTrack
OpenVR / SteamVR driver allowing head tracking with any [OpenTrack](https://github.com/opentrack/opentrack) trackers, for DIY VR headset made of Android smartphone or [HDMI display](http://ali.pub/1llt51) and tracker.<br>
<br>OpenTrack supports the following trackers: FreePie UDP receiver (FreePie IMU for Android), Hatire Arduino or [Razor IMU](https://github.com/Razor-AHRS/razor-9dof-ahrs) ([Arduino](http://ali.pub/1lltzk) + [GY-85](http://ali.pub/1lltk0)), [Oculus DK1](http://ali.pub/1llqtf), Aruco (Paper + [WebCam](http://ali.pub/2k9jf6)) and etc.<br>
<br>[![youtube-freetrack](https://user-images.githubusercontent.com/9499881/32277549-411d313c-bf2c-11e7-9b07-77a903783cf5.gif)](https://youtu.be/mDkdj_vn5Lk)

## Setup
1. Install [SteamVR](https://store.steampowered.com/app/250820/SteamVR/).
2. Unpack SteamVR FreeTrack or UDP driver to folder "...\Steam\steamapps\common\SteamVR\drivers" (both are supported in OpenTrack, I recommend using FreeTrack). If necessary, configure the settings in the configuration file "...\Steam\steamapps\common\SteamVR\drivers\opentrack\resources\settings\default.vrsettings".
3. Download, install and configure [OpenTrack](https://github.com/opentrack/opentrack) (add hot centering key, disable filter, change output interface "freetrack 2.0 Enhanced" or "UDP over network", depending on the selected driver). If you selected UDP, then in the output interface settings you need to set IP "127.0.0.1".
4. SteamVR starts and in the SteamVR status window click -> Room settings -> Small room -> Calibration -> 170 cm. The SteamVR tutorial can be closed and SteamVR Home is disabled in the SteamVR settings.
5. Put on HMD and controllers, then centering them if necessary.

If you are using an Android smartphone, you need to use FreePie IMU from the OpenTrack archive to tracking and you can use the "Moonlight" for smartphone and [Sunshine](https://github.com/loki-47-6F-64/sunshine/releases) or [Open-Stream](https://open-stream.net/) for PC to stream picture from the monitor screen. Also you can use this [guide](https://stackoverflow.com/a/46433454).<br><br>
If you use the [Arduino Razor IMU tracker](https://github.com/Razor-AHRS/razor-9dof-ahrs), you can use [this driver](https://github.com/r57zone/OpenVR-ArduinoHMD), with its SteamVR driver or the [Razor IMU SteamVR](https://github.com/r57zone/VR-tracking-apps/releases) application.

## Configuration file options
Name | Description
------------ | -------------
DistanceBetweenEyes | The distance between stereo images, the larger the closer.
DistortionK1, DistortionK2 | Lens distortion factors.
ScreenOffsetX | Horizontal image shift.
ZoomHeight, ZoomWidth | Scaling factors of stereo images.
FOV | Degree of field of view. You can zoom in, depending on the VR headset lenses.
displayFrequency | Screen refresh rate.
renderWidth, renderHeight | Image rendering resolution for one eye.
windowWidth, windowHeight | Height and width of the displayed window.
windowX, windowY | Window offset is required for display on other monitors. For example, to display on the second display, which is displayed on the right, you need to specify the value 1920 (provided that the first display has a resolution of 1920 by 1080). The exact data can be viewed using the [MultiMonitorTool utility](https://www.nirsoft.net/utils/multi_monitor_tool.html), and also with it you can turn off and turn on the second monitor via a bat file.
DebugMode | Debug mode, locked at 30 FPS. After checking, it is recommended to set it to false (disable).

## Known Issues
1. Red screen. You can fix this by selecting the "Headset Window" window.
2. The keyboard, mouse and gamepad do not work when using the driver on one monitor (The game window should be in focus). On systems with multiple monitors, you can move the application to second monitor and select it, then everything will work.

## Download
>Version for x86 и x64.<br>
**[Download](https://github.com/r57zone/OpenVR-OpenTrack/releases)**

## Feedback
`r57zone[at]gmail.com`