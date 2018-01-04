object Main: TMain
  Left = 190
  Top = 123
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'SteamVR Settings'
  ClientHeight = 197
  ClientWidth = 292
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
    Left = 214
    Top = 46
    Width = 5
    Height = 13
    Caption = 'x'
  end
  object ChsDriverLbl: TLabel
    Left = 8
    Top = 82
    Width = 71
    Height = 13
    Caption = 'Choose driver: '
  end
  object DisplayLbl: TLabel
    Left = 272
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
  object DisplayTB: TTrackBar
    Left = 192
    Top = 8
    Width = 73
    Height = 30
    Max = 1
    TabOrder = 0
    OnChange = DisplayTBChange
  end
  object RndWidthEdt: TEdit
    Left = 152
    Top = 43
    Width = 49
    Height = 21
    TabOrder = 1
    Text = '0'
  end
  object RndHeightEdt: TEdit
    Left = 232
    Top = 43
    Width = 49
    Height = 21
    TabOrder = 2
    Text = '0'
  end
  object FreeTrackRB: TRadioButton
    Left = 104
    Top = 80
    Width = 73
    Height = 17
    Caption = 'FreeTrack'
    Checked = True
    TabOrder = 3
    TabStop = True
  end
  object UDPRB: TRadioButton
    Left = 176
    Top = 80
    Width = 106
    Height = 17
    Caption = 'UDP over network'
    TabOrder = 4
  end
  object ApplyBtn: TButton
    Left = 8
    Top = 163
    Width = 75
    Height = 25
    Caption = 'Apply'
    TabOrder = 6
    OnClick = ApplyBtnClick
  end
  object CancelBtn: TButton
    Left = 88
    Top = 163
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 7
    OnClick = CancelBtnClick
  end
  object AboutBtn: TButton
    Left = 256
    Top = 163
    Width = 28
    Height = 25
    Caption = '?'
    TabOrder = 8
    OnClick = AboutBtnClick
  end
  object DbgMdCb: TCheckBox
    Left = 224
    Top = 112
    Width = 60
    Height = 17
    Caption = 'Activate'
    TabOrder = 5
  end
  object XPManifest: TXPManifest
    Left = 224
    Top = 163
  end
end
