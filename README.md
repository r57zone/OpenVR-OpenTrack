[![EN](https://user-images.githubusercontent.com/9499881/27683803-659dc988-5cd8-11e7-9c05-0b747e917666.png)](https://github.com/r57zone/OpenVR-OpenTrack/blob/master/README.md) 
[![RU](https://user-images.githubusercontent.com/9499881/27683795-5b0fbac6-5cd8-11e7-929c-057833e01fb1.png)](https://github.com/r57zone/OpenVR-OpenTrack/blob/master/README.RU.md) 
# OpenVR OpenTrack
OpenVR / SteamVR driver allowing head tracking with any [OpenTrack](https://github.com/opentrack/opentrack) trackers, for DIY VR headset made of Android smartphone or [HDMI display](http://ali.pub/1llt51) and tracker.<br>
<br>OpenTrack supports the following trackers: FreePie UDP receiver (FreePie IMU для Android), Hatire Arduino ([Arduino](http://ali.pub/1lltzk) + [GY-85](http://ali.pub/1lltk0)), [Oculus DK1](http://ali.pub/1llqtf), Aruco (Paper + WebCam) and etc.<br>
<br>![](https://user-images.githubusercontent.com/9499881/27535649-d8822f38-5a7c-11e7-8681-4e42ded2eb1c.gif)<br>
[(YouTube)](https://youtu.be/r-xJ0oMcltY)<br>

## Setup
1. Start "SteamVR Settings", select the monitor number and driver type "FreeTrack" or "UDP over network" (both are supported in OpenTrack, I recommend using FreeTrack). Also, if you do not have a high-performance computer, you can change the rendering resolution.
2. Download, install and configure [OpenTrack](https://github.com/opentrack/opentrack) (add hot centering key, change output interface "freetrack 2.0 Enhanced" or "UDP over network", depending on the selected driver. If you selected UDP, then in the output interface settings you need to set IP "127.0.0.1".<br><br>

If you are using an Android smartphone, you need to use FreePie IMU from the OpenTrack archive to tracking and you can use the "Moonlight" application (for Nvidia 600 series only and above) or for any other application to stream pictures from the monitor screen.<br><br>
If you use the Arduino Razor IMU tracker, you can use the standard [TrueOpenVR](https://github.com/TrueOpenVR), with its SteamVR driver or the [Razor IMU SteamVR](https://github.com/r57zone/VR-tracking-apps/releases) application.

## Download
>Version for x86 и x64.<br>
**[Download](https://github.com/r57zone/OpenVR-OpenTrack/releases)**<br>

## Feedback
`r57zone[at]gmail.com`