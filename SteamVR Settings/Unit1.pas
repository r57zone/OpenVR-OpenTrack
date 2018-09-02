unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, XPMan, Registry, ExtCtrls;

type
  TMain = class(TForm)
    DisplayTB: TTrackBar;
    ChsDisplay: TLabel;
    RndResLbl: TLabel;
    RndWidthEdt: TEdit;
    XLbl: TLabel;
    RndHeightEdt: TEdit;
    FreeTrackRB: TRadioButton;
    UDPRB: TRadioButton;
    ChsDriverLbl: TLabel;
    XPManifest: TXPManifest;
    DisplayLbl: TLabel;
    DbgMdCb: TCheckBox;
    DbgMdLbl: TLabel;
    MoreSettingsLbl: TLabel;
    DistortionLbl: TLabel;
    DistortionK1Edt: TEdit;
    DistortionK2Edt: TEdit;
    ZoomLbl: TLabel;
    ZoomWidthEdt: TEdit;
    ZoomHeightEdt: TEdit;
    AboutBtn: TButton;
    CancelBtn: TButton;
    InstallBtn: TButton;
    ChangeDisplayFrequencyCB: TCheckBox;
    DisplayFrequencyEdt: TEdit;
    ghzLbl: TLabel;
    DistanceEyesLbl: TLabel;
    IPDEdt: TEdit;
    DistanceEyesEdt: TEdit;
    IPDLbl: TLabel;
    mmLbl: TLabel;
    PixelsLbl: TLabel;
    UninstallBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure AboutBtnClick(Sender: TObject);
    procedure DisplayTBChange(Sender: TObject);
    procedure InstallBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure MoreSettingsLblClick(Sender: TObject);
    procedure ChangeDisplayFrequencyCBClick(Sender: TObject);
    procedure UninstallBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Main: TMain;
  SteamPath: string;

implementation

{$R *.dfm}

procedure TMain.FormCreate(Sender: TObject);
var
  Reg: TRegistry;
begin
  Application.Title:=Caption;
  Width:=290;

  Reg:=TRegistry.Create;
  Reg.RootKey:=HKEY_CURRENT_USER;
  if (Reg.OpenKey('\Software\Valve\Steam', false)) then
    SteamPath:=StringReplace(Reg.ReadString('SteamPath'), '/', '\', [rfReplaceAll]);
  Reg.CloseKey;
  Reg.Free;

  SetWindowLong(RndWidthEdt.Handle, GWL_STYLE, GetWindowLong(RndWidthEdt.Handle, GWL_STYLE) or ES_NUMBER);
  SetWindowLong(RndHeightEdt.Handle, GWL_STYLE, GetWindowLong(RndHeightEdt.Handle, GWL_STYLE) or ES_NUMBER);
  SetWindowLong(DisplayFrequencyEdt.Handle, GWL_STYLE, GetWindowLong(DisplayFrequencyEdt.Handle, GWL_STYLE) or ES_NUMBER);
  SetWindowLong(DistanceEyesEdt.Handle, GWL_STYLE, GetWindowLong(DistanceEyesEdt.Handle, GWL_STYLE) or ES_NUMBER);

  DisplayTB.Max:=Screen.MonitorCount - 1;
  RndWidthEdt.Text:=IntToStr(Screen.Monitors[0].Width);
  RndHeightEdt.Text:=IntToStr(Screen.Monitors[0].Height);

  DbgMdLbl.Caption:=DbgMdLbl.Caption + #13#10 + 'Windowed borderless fullscreen' + #13#10 + 'with lock to 30 FPS.';
  DistortionLbl.Caption:=DistortionLbl.Caption + #13#10 + 'Lens distortion for output view.';
  ZoomLbl.Caption:=ZoomLbl.Caption + #13#10 + 'Eyes output view sizes.' + #13#10 + 'The larger the value, the smaller the output view.';
end;

procedure TMain.AboutBtnClick(Sender: TObject);
begin
  Application.MessageBox('OpenVR OpenTrack' + #13#10 +
  'https://github.com/r57zone/OpenVR-OpenTrack' + #13#10 +
  'r57zone@gmail.com', PChar(Caption), MB_ICONINFORMATION);
end;

procedure TMain.DisplayTBChange(Sender: TObject);
begin
  RndWidthEdt.Text:=IntToStr(Screen.Monitors[DisplayTB.Position].Width);
  RndHeightEdt.Text:=IntToStr(Screen.Monitors[DisplayTB.Position].Height);
  DisplayLbl.Caption:=IntToStr(DisplayTB.Position + 1);
end;

procedure TMain.InstallBtnClick(Sender: TObject);
var
  Config: TStringList;
  Error: boolean;
begin
  Error:=false;

  if DirectoryExists(SteamPath) then begin

    //Обновлнение и копирование конфига OpenVR
    if FileExists(ExtractFilePath(ParamStr(0)) + 'OpenVR\steamvr.vrsettings') then begin
      Config:=TStringList.Create;
      Config.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'OpenVR\steamvr.vrsettings');

      if (ChangeDisplayFrequencyCB.Checked) and (DisplayFrequencyEdt.Text <> '0') then
        Config.Text:=StringReplace(Config.Text, '<WINDOWY>,', '<WINDOWY>,' + #13#10 + #9 + '  "displayFrequency" : ' + DisplayFrequencyEdt.Text + ',', [rfReplaceAll]);

      Config.Text:=StringReplace(Config.Text, '<RENDERWIDTH>', RndWidthEdt.Text, [rfReplaceAll]);
      Config.Text:=StringReplace(Config.Text, '<RENDERHEIGHT>', RndHeightEdt.Text, [rfReplaceAll]);

      Config.Text:=StringReplace(Config.Text, '<WINDOWWIDTH>', IntToStr(Screen.Monitors[DisplayTB.Position].Width), [rfReplaceAll]);
      Config.Text:=StringReplace(Config.Text, '<WINDOWHEIGHT>', IntToStr(Screen.Monitors[DisplayTB.Position].Height), [rfReplaceAll]);

      Config.Text:=StringReplace(Config.Text, '<WINDOWX>', IntToStr(Screen.Monitors[DisplayTB.Position].Left), [rfReplaceAll]);
      Config.Text:=StringReplace(Config.Text, '<WINDOWY>', IntToStr(Screen.Monitors[DisplayTB.Position].Top), [rfReplaceAll]);

      Config.Text:=StringReplace(Config.Text, '<DISTORTIONK1>', DistortionK1Edt.Text, [rfReplaceAll]);
      Config.Text:=StringReplace(Config.Text, '<DISTORTIONK2>', DistortionK2Edt.Text, [rfReplaceAll]);

      Config.Text:=StringReplace(Config.Text, '<ZOOMWIDTH>', ZoomWidthEdt.Text, [rfReplaceAll]);
      Config.Text:=StringReplace(Config.Text, '<ZOOMHEIGHT>', ZoomHeightEdt.Text, [rfReplaceAll]);

      Config.Text:=StringReplace(Config.Text, '<IPD>', IPDEdt.Text, [rfReplaceAll]);

      Config.Text:=StringReplace(Config.Text, '<DISTANCEEYES>', DistanceEyesEdt.Text, [rfReplaceAll]);

      if DbgMdCb.Checked then
        Config.Text:=StringReplace(Config.Text, '<DEBUGMODE>', 'true', [rfReplaceAll])
      else
        Config.Text:=StringReplace(Config.Text, '<DEBUGMODE>', 'false', [rfReplaceAll]);

      Config.SaveToFile(SteamPath + '\config\steamvr.vrsettings');

      Config.Free;
    end else begin
      Application.MessageBox('File "steamvr.vrsettings" not found.', PChar(Caption), MB_ICONERROR);
      Error:=true;
    end;

    //Копирование драйверов
    if UDPRB.Checked then begin  //UDP

      if not ((CopyFile(PChar(ExtractFilePath(ParamStr(0)) + 'OpenVR\DriverUDP32.dll'), PChar(SteamPath + '\steamapps\common\SteamVR\drivers\null\bin\win32\driver_null.dll'), false)) and
      (CopyFile(PChar(ExtractFilePath(ParamStr(0)) + 'OpenVR\DriverUDP64.dll'), PChar(SteamPath + '\steamapps\common\SteamVR\drivers\null\bin\win64\driver_null.dll'), false))) then begin
        Application.MessageBox('Error copy driver files. Please close Steam and SteamVR.', PChar(Caption), MB_ICONERROR);
        Error:=true;
      end;

    end else begin //FreeTrack

      if not ((CopyFile(PChar(ExtractFilePath(ParamStr(0)) + 'OpenVR\DriverFreeTrack32.dll'), PChar(SteamPath + '\steamapps\common\SteamVR\drivers\null\bin\win32\driver_null.dll'), false)) and
      (CopyFile(PChar(ExtractFilePath(ParamStr(0)) + 'OpenVR\DriverFreeTrack64.dll'), PChar(SteamPath + '\steamapps\common\SteamVR\drivers\null\bin\win64\driver_null.dll'), false))) then begin
        Application.MessageBox('Error copy driver files. Please close Steam and SteamVR.', PChar(Caption), MB_ICONERROR);
        Error:=true;
      end;
    end;

    if Error = false then
      Application.MessageBox('Installed', PChar(Caption), MB_ICONINFORMATION);

  end else
    ShowMessage('Steam not found. Please install Steam and SteamVR');
  
  if Error = false then
    Close;
end;

procedure TMain.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TMain.MoreSettingsLblClick(Sender: TObject);
begin
  if Width = 290 then
    Width:=608
  else
    Width:=290;
end;

procedure TMain.ChangeDisplayFrequencyCBClick(Sender: TObject);
begin
  if ChangeDisplayFrequencyCB.Checked then
    DisplayFrequencyEdt.Enabled:=true
  else
    DisplayFrequencyEdt.Enabled:=false;
end;

procedure TMain.UninstallBtnClick(Sender: TObject);
begin
  if DirectoryExists(SteamPath) then begin
    if FileExists(SteamPath + '\config\steamvr.vrsettings') then
      DeleteFile(SteamPath + '\config\steamvr.vrsettings');
      Application.MessageBox('Uninstalled', PChar(Caption), MB_ICONINFORMATION);
  end else
    Application.MessageBox('Steam not found. Please install Steam and SteamVR', PChar(Caption), MB_ICONERROR);
end;

end.
