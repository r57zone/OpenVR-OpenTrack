# (En) OpenVR OpenTrack
OpenVR / SteamVR driver, allowing you to rotate your head, with the help of any [OpenTrack](https://github.com/opentrack/opentrack) trackers, for DIY VR headset made of Android smartphone or [HDMI display](http://ali.pub/1llt51) and tracker.<br>
OpenTrack supports the following trackers: FreePie UDP receiver (FreePie IMU для Android), Hatire Arduino ([Arduino](http://ali.pub/1lltzk) + [GY-85](http://ali.pub/1lltk0)), [Oculus DK1](http://ali.pub/1llqtf), Aruco (Paper + WebCam) and etc.

## Setup
1. Move and replace the "null" folder, from the archive, to the folder "...\Steam\steamapps\common\SteamVR\drivers\null".
2. Move and replace the file "steamvr.vrsettings", from the archive, in the folder "...\Steam\config".
3. Download, install and configure [OpenTrack](https://github.com/opentrack/opentrack).
If you are using an Android smartphone, need to use FreePie IMU from the OpenTrack archive for head tracking, and you can use the Moonlight application (for Nvidia 600 series only and above) or any other application to stream the picture from the monitor screen.<br>
If you use a DIY VR headset that works as a second monitor, then after launching SteamVR, press the "Shift" + "Win" + "Right" keys to move the application to the second monitor.<br>

## Issues
Input lag of head tracking (need self thread for [read WinSock](https://github.com/r57zone/OpenVR-OpenTrack/blob/master/OpenVR/samples/driver_sample/driver_sample.cpp#L418-L428)).<br>

## Download
>Version for x86 и x64.<br>
**[Download](https://github.com/r57zone/OpenVR-OpenTrack)**<br>

## Feedback
`r57zone[at]gmail.com`

# (Ru) OpenVR OpenTrack
Драйвер для OpenVR / SteamVR, позволяющий вращать головой, с помощью любого [OpenTrack](https://github.com/opentrack/opentrack) трекера, для самодельного VR шлема из Android смартфона или [HDMI дисплея](http://ali.pub/1llt51) и трекера.<br>
OpenTrack поддерживает следующие трекеры: FreePie UDP receiver (FreePie IMU для Android), Hatire Arduino ([Arduino](http://ali.pub/1lltzk) + [GY-85](http://ali.pub/1lltk0)), [Oculus DK1](http://ali.pub/1llqtf), Aruco (Paper + WebCam) и другие.

## Настройка 
1. Переместить и заменить папку "null", из архива, в папку "...\Steam\steamapps\common\SteamVR\drivers\null".
2. Переместить и заменить файл "steamvr.vrsettings", из архива, в папке "...\Steam\config".
3. Загрузить, установить и настроить [OpenTrack](https://github.com/opentrack/opentrack).
Если вы используете Android смартфон, то для трекинга необходимо использовать FreePie IMU, из архива OpenTrack, а для стриминга картинки, с экрана монитора, можно использовать приложение Moonlight (только для Nvidia 600 серии и выше) или любое другое приложение.<br>
Если вы используете самодельный VR шлем, который работает как второй монитор, то после запуска SteamVR нажмите клавиши "Shift" + "Win" + "Вправо" для переноса приложения на второй монитор.<br>

## Проблемы
Тормозит отслеживание (нужен отдельный самостоятельный поток для [чтения WinSock](https://github.com/r57zone/OpenVR-OpenTrack/blob/master/OpenVR/samples/driver_sample/driver_sample.cpp#L418-L428)).<br>

## Загрузка
>Версия для x86 и x64.<br>
**[Загрузить](https://github.com/r57zone/OpenVR-OpenTrack)**<br>

## Обратная связь
`r57zone[собака]gmail.com`