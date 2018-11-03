object Main: TMain
  Left = 190
  Top = 123
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'SteamVR Settings'
  ClientHeight = 227
  ClientWidth = 608
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ChsDisplay: TLabel
    Left = 8
    Top = 12
    Width = 95
    Height = 13
    Caption = 'Choose VR display: '
  end
  object RndResLbl: TLabel
    Left = 8
    Top = 46
    Width = 89
    Height = 13
    Caption = 'Render resolution :'
  end
  object XLbl: TLabel
    Left = 206
    Top = 46
    Width = 5
    Height = 13
    Caption = 'x'
  end
  object ChsDriverLbl: TLabel
    Left = 8
    Top = 81
    Width = 71
    Height = 13
    Caption = 'Choose driver: '
  end
  object DisplayLbl: TLabel
    Left = 264
    Top = 12
    Width = 6
    Height = 13
    Caption = '1'
  end
  object DbgMdLbl: TLabel
    Left = 8
    Top = 112
    Width = 161
    Height = 13
    Caption = 'Debug mode (not recommended): '
  end
  object MoreSettingsLbl: TLabel
    Left = 104
    Top = 163
    Width = 72
    Height = 13
    Cursor = crHandPoint
    Caption = 'More settings...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = MoreSettingsLblClick
  end
  object DistortionLbl: TLabel
    Left = 304
    Top = 12
    Width = 120
    Height = 13
    Caption = 'Distortion coefficient 1, 2:'
  end
  object ZoomLbl: TLabel
    Left = 304
    Top = 54
    Width = 93
    Height = 13
    Caption = 'Zoom width, height:'
  end
  object ghzLbl: TLabel
    Left = 565
    Top = 183
    Width = 17
    Height = 13
    Caption = 'ghz'
  end
  object DistanceEyesLbl: TLabel
    Left = 304
    Top = 109
    Width = 196
    Height = 13
    Caption = 'Zoom out the distance between the eyes:'
  end
  object PixelsLbl: TLabel
    Left = 565
    Top = 109
    Width = 26
    Height = 13
    Caption = 'pixels'
  end
  object IPDLbl: TLabel
    Left = 304
    Top = 145
    Width = 132
    Height = 13
    Caption = 'Interpupillary distance (IPD):'
  end
  object mmLbl: TLabel
    Left = 565
    Top = 146
    Width = 16
    Height = 13
    Caption = 'mm'
  end
  object DisplayTB: TTrackBar
    Left = 184
    Top = 8
    Width = 73
    Height = 30
    Max = 1
    TabOrder = 0
    OnChange = DisplayTBChange
  end
  object RndWidthEdt: TEdit
    Left = 144
    Top = 43
    Width = 49
    Height = 21
    TabOrder = 1
    Text = '0'
  end
  object RndHeightEdt: TEdit
    Left = 224
    Top = 43
    Width = 49
    Height = 21
    TabOrder = 2
    Text = '0'
  end
  object FreeTrackRB: TRadioButton
    Left = 96
    Top = 80
    Width = 73
    Height = 17
    Caption = 'FreeTrack'
    Checked = True
    TabOrder = 3
    TabStop = True
  end
  object UDPRB: TRadioButton
    Left = 168
    Top = 80
    Width = 106
    Height = 17
    Caption = 'UDP over network'
    TabOrder = 4
  end
  object DbgMdCb: TCheckBox
    Left = 216
    Top = 119
    Width = 60
    Height = 17
    Caption = 'Activate'
    TabOrder = 5
  end
  object DistortionK1Edt: TEdit
    Left = 472
    Top = 14
    Width = 57
    Height = 21
    TabOrder = 10
    Text = '0.91'
  end
  object DistortionK2Edt: TEdit
    Left = 536
    Top = 13
    Width = 57
    Height = 21
    TabOrder = 11
    Text = '0.93'
  end
  object ZoomWidthEdt: TEdit
    Left = 472
    Top = 55
    Width = 57
    Height = 21
    TabOrder = 12
    Text = '0.8'
  end
  object ZoomHeightEdt: TEdit
    Left = 536
    Top = 55
    Width = 57
    Height = 21
    TabOrder = 13
    Text = '0.8'
  end
  object AboutBtn: TButton
    Left = 248
    Top = 192
    Width = 28
    Height = 25
    Caption = '?'
    TabOrder = 9
    OnClick = AboutBtnClick
  end
  object CancelBtn: TButton
    Left = 168
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 8
    OnClick = CancelBtnClick
  end
  object InstallBtn: TButton
    Left = 8
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Install'
    TabOrder = 6
    OnClick = InstallBtnClick
  end
  object ChangeDisplayFrequencyCB: TCheckBox
    Left = 304
    Top = 181
    Width = 145
    Height = 17
    Caption = 'Change display frequency'
    Checked = True
    State = cbChecked
    TabOrder = 16
    OnClick = ChangeDisplayFrequencyCBClick
  end
  object DisplayFrequencyEdt: TEdit
    Left = 512
    Top = 179
    Width = 49
    Height = 21
    TabOrder = 17
    Text = '60'
  end
  object IPDEdt: TEdit
    Left = 512
    Top = 143
    Width = 49
    Height = 21
    TabOrder = 15
    Text = '0.065'
  end
  object DistanceEyesEdt: TEdit
    Left = 512
    Top = 106
    Width = 49
    Height = 21
    TabOrder = 14
    Text = '0'
  end
  object UninstallBtn: TButton
    Left = 88
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Uninstall'
    TabOrder = 7
    OnClick = UninstallBtnClick
  end
  object XPManifest: TXPManifest
    Left = 160
    Top = 3
  end
end
