# (En) OpenVR OpenTrack
OpenVR / SteamVR driver allowing head tracking with any [OpenTrack](https://github.com/opentrack/opentrack) trackers, for DIY VR headset made of Android smartphone or [HDMI display](http://ali.pub/1llt51) and tracker.<br>
<br>OpenTrack supports the following trackers: FreePie UDP receiver (FreePie IMU для Android), Hatire Arduino ([Arduino](http://ali.pub/1lltzk) + [GY-85](http://ali.pub/1lltk0)), [Oculus DK1](http://ali.pub/1llqtf), Aruco (Paper + WebCam) and etc.<br>
<br>![](https://user-images.githubusercontent.com/9499881/27535649-d8822f38-5a7c-11e7-8681-4e42ded2eb1c.gif)<br>
[(YouTube)](https://youtu.be/r-xJ0oMcltY)<br>

## Setup
1. Move with replace the "null" folder, from the archive, to the folder "...\Steam\steamapps\common\SteamVR\drivers".
2. Move with replace the file "steamvr.vrsettings", from the archive, in the folder "...\Steam\config".
3. Download, install and configure [OpenTrack](https://github.com/opentrack/opentrack) (disable filter, add hotkey to centring, setup sensitive, change output to "UDP over network" and set IP to "127.0.0.1" on output plugin settings).<br><br>
If you are using an Android smartphone, need to use FreePie IMU from the OpenTrack archive for head tracking, and you can use the Moonlight application (for Nvidia 600 series only and above) or any other application to stream the picture from the monitor screen.<br><br>
If you use a VR headset that works as a second monitor, then use the [MultiMonitorTool](http://www.nirsoft.net/utils/multi_monitor_tool.html) program, which allows you to quickly change the "Primary Monitor", for starts fullscreen applications such as SteamVR. In the archive there are batch files, correct them if necessary, create shortcuts and set hotkeys. If your icons get lost, then use the program [ReIcon](http://www.sordum.org/8366/reicon-v1-7-restore-desktop-icon-layouts/) to save and restore the positions of the icons.<br>

## Download
>Version for x86 и x64.<br>
**[Download](https://github.com/r57zone/OpenVR-OpenTrack/releases)**<br>

## Feedback
`r57zone[at]gmail.com`<br>

# (Ru) OpenVR OpenTrack
Драйвер для OpenVR / SteamVR, позволяющий вращать головой, с помощью любого [OpenTrack](https://github.com/opentrack/opentrack) трекера, для самодельного VR шлема из Android смартфона или [HDMI дисплея](http://ali.pub/1llt51) и трекера.<br>
<br>OpenTrack поддерживает следующие трекеры: FreePie UDP receiver (FreePie IMU для Android), Hatire Arduino ([Arduino](http://ali.pub/1lltzk) + [GY-85](http://ali.pub/1lltk0)), [Oculus DK1](http://ali.pub/1llqtf), Aruco (Paper + WebCam) и другие.<br>

## Настройка 
1. Переместить, с заменой, папку "null", из архива, в папку "...\Steam\steamapps\common\SteamVR\drivers".
2. Переместить, с заменой, файл "steamvr.vrsettings", из архива, в папку "...\Steam\config".
3. Загрузить, установить и настроить [OpenTrack](https://github.com/opentrack/opentrack) (добавить горячую клавишу для сброса центровки, настроить чувствительность, изменить выходной интерфейс на "UDP over network" и задать IP "127.0.0.1" в настройках выходного интерфейса).<br><br>
Если вы используете Android смартфон, то для трекинга необходимо использовать FreePie IMU, из архива OpenTrack, а для стриминга картинки, с экрана монитора, можно использовать приложение Moonlight (только для Nvidia 600 серии и выше) или любое другое приложение.<br><br>
Если вы используете VR шлем, который работает как второй монитор, то воспользуйтесь программой [MultiMonitorTool](http://www.nirsoft.net/utils/multi_monitor_tool.html), с помощью которой можно быстро изменить "Первичый монитор", именно на нем запускается полноэкранные приложения, такие как SteamVR. В архиве есть командные файлы (bat), исправьте их если нужно, создайте ярлыки и задайте горячие клавиши. Если ваши иконки сбиваются, то используйте программу [ReIcon](http://www.sordum.org/8366/reicon-v1-7-restore-desktop-icon-layouts/) для сохранения и восстановления позиций иконок.<br>

## Загрузка
>Версия для x86 и x64.<br>
**[Загрузить](https://github.com/r57zone/OpenVR-OpenTrack/releases)**<br>

## Обратная связь
`r57zone[собака]gmail.com`