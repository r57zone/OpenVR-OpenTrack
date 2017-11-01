[![EN](https://user-images.githubusercontent.com/9499881/27683803-659dc988-5cd8-11e7-9c05-0b747e917666.png)](https://github.com/r57zone/OpenVR-OpenTrack/blob/master/README.md) 
[![RU](https://user-images.githubusercontent.com/9499881/27683795-5b0fbac6-5cd8-11e7-929c-057833e01fb1.png)](https://github.com/r57zone/OpenVR-OpenTrack/blob/master/README.RU.md) 
# OpenVR OpenTrack
Драйвер для OpenVR / SteamVR, позволяющий вращать головой, с помощью любого [OpenTrack](https://github.com/opentrack/opentrack) трекера, для самодельного VR шлема из Android смартфона или [HDMI дисплея](http://ali.pub/1llt51) и трекера.<br>
<br>OpenTrack поддерживает следующие трекеры: FreePie UDP receiver (FreePie IMU для Android), Hatire Arduino ([Arduino](http://ali.pub/1lltzk) + [GY-85](http://ali.pub/1lltk0)), [Oculus DK1](http://ali.pub/1llqtf), Aruco (Paper + WebCam) и другие.<br>
<br>[![youtube-freetrack](https://user-images.githubusercontent.com/9499881/32277549-411d313c-bf2c-11e7-9b07-77a903783cf5.gif)](https://youtu.be/mDkdj_vn5Lk)

## Настройка 
1. Запустить "SteamVR Settings", выбрать номер монитора и тип драйвера "FreeTrack" или "UDP over network" (оба поддерживаются в OpenTrack, рекомендую использовать FreeTrack). Также если у вас недостаточно производительный компьютер, то можно уменьшить разрешение рендера.
2. Загрузить, установить и настроить [OpenTrack](https://github.com/opentrack/opentrack) (добавить горячую клавишу центрирования, выключить фильтр, изменить выходной интерфейс "freetrack 2.0 Enhanced" или на "UDP over network", в зависимости от выбранного драйвера). Если вы выбрали UDP, то в настройках выходного интерфейса нужно задать IP "127.0.0.1".<br><br>

Если вы используете Android смартфон, то для трекинга необходимо использовать FreePie IMU, из архива OpenTrack, а для стриминга картинки с экрана монитора, можно использовать приложение "Moonlight" (только для Nvidia 600 серии и выше) или любое другое приложение.<br><br>
Если вы используете Arduino Razor IMU трекер, вы можете использовать стандарт [TrueOpenVR](https://github.com/TrueOpenVR), вместе с его SteamVR драйвером или приложение [Razor IMU SteamVR](https://github.com/r57zone/VR-tracking-apps/releases).

## Загрузка
>Версия для x86 и x64.<br>
**[Загрузить](https://github.com/r57zone/OpenVR-OpenTrack/releases)**<br>

## Обратная связь
`r57zone[собака]gmail.com`